{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad (unless, when)
import Control.Monad.Writer.CPS
import Data.Foldable
import Data.Functor (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Dot ()
import System.Directory
import System.Process.Typed (proc, runProcess_)

main :: IO ()
main = do
  p <- canonicalizePath "../runtime/src/apply.zig"
  T.writeFile
    p
    $ runEmit do
      emit "// This file was generated! Do not edit!"
      emit "const std = @import(\"std\");"
      emit "const debug = std.debug;"
      emit "const object = @import(\"object.zig\");"
      emit "const Object = object.Object;"
      emit "const Allocator = std.mem.Allocator;"
      emit "const Closure = object.Closure;"
      emit "const Pap = object.Pap;"
      emit "const Box = object.Box;"
      for_ ns genBoxedFnTy
      emit "pub const FnBoxedN = fn(f: *Closure, args: [*]Box) Box;"
      for_ ns genCallClosureBoxed
      for_ ns genCallPapBoxed
      for_ ns genApplyClosureBoxed
      for_ ns genApplyBoxed
      genCallN
      genApplyN
  runProcess_ (proc "zig" ["fmt", p])
  where
    ns = [1 :: Int .. maxCallSize]

data Rep = Boxed

genCallN :: M ()
genCallN = do
  emit "pub fn call_closure_boxed_n(f: *Closure, args: []Box) Box"
  braces do
    emit "switch (args.len)"
    braces do
      for_ [1 :: Int .. maxCallSize] \i -> branch (show i).t do
        emit $ "return call_closure_boxed_" <> (show i).t
        parens do
          emit "f,"
          commaList [0 :: Int .. i - 1] \j -> do
            emit $ "args[" <> (show j).t <> "]"
        emit ";"
      branch "else" do
        emit $ "debug.assert(f.arity() > " <> (show maxCallSize).t <> ");"
        emit "const code: *FnBoxedN = @ptrCast(f.code);"
        emit "return code(f, args.ptr);"

genApplyN :: M ()
genApplyN = do
  emit "pub fn apply_boxed_n(f: *Object, args: []Box) Box"
  braces do
    emit "switch (args.len)"
    braces do
      for_ [1 :: Int .. maxCallSize] \i -> branch (show i).t do
        emit $ "return apply_boxed_" <> (show i).t
        parens do
          emit "f,"
          commaList [0 :: Int .. i - 1] \j -> do
            emit $ "args[" <> (show j).t <> "]"
        emit ";"
      branch "else" do
        emit $ "const res = apply_boxed_" <> (show maxCallSize).t
        parens do
          emit "f,"
          commaList [0 .. maxCallSize - 1] \j -> do
            emit $ "args[" <> (show j).t <> "]"
        emit ";"
        emit $ "return apply_boxed_n(res.as_object(), args[16..]);"

stmt :: M a -> M a
stmt m = do
  x <- m
  emit ";"
  pure x

genCallClosureBoxed :: Int -> M ()
genCallClosureBoxed n = do
  emit $ "pub inline fn call_closure_boxed_" <> (show n).t
  parens do
    emit "f: *Closure,"
    enumParams "arg" n
  emit "Box"
  braces do
    emit $ "const code: *FnBoxed" <> (show n).t <> "= @ptrCast(f.code);"
    stmt do
      emit "return code"
      parens do
        emit "f,"
        enumArgs "arg" n

enumArgs :: Text -> Int -> M ()
enumArgs prefix n = enumArgsFrom prefix 0 n

enumArgsFrom :: Text -> Int -> Int -> M ()
enumArgsFrom prefix i j =
  commaList [i :: Int .. j - 1] \i -> do
    emit $ prefix <> (show i).t

enumParams :: Text -> Int -> M ()
enumParams prefix n = commaList [0 :: Int .. n - 1] \i -> do
  emit $ prefix <> (show i).t <> ": Box"

genBoxedFnTy :: Int -> M ()
genBoxedFnTy n = do
  emit $ "pub const FnBoxed" <> (show n).t <> " = fn"
  parens do
    emit "f: *Closure,"
    enumParams "arg" n
  emit "Box"
  emit ";"

maxCallSize :: Int
maxCallSize = 16

genCallPapBoxed :: Int -> M ()
genCallPapBoxed n = do
  emit $ "pub fn apply_pap_boxed_" <> (show n).t
  parens do
    emit "pap: *Pap,"
    enumParams "arg" n
  emit "Box"
  braces do
    emit "const closure = pap.closure;"
    emit "const arity = pap.arity();"
    emit "const fixed = pap.fixed();"

    emit $ "if (arity == fixed.len + " <> (show n).t <> ")"
    braces do
      emit "switch (arity)"
      braces do
        for_ [n .. maxCallSize] \i -> do
          branch (show i).t do
            emit $ "return call_closure_boxed_" <> (show i).t
            parens do
              emit "closure,"
              let fixedLen = i - n
              unless (fixedLen == 0) do
                for_ [0 :: Int .. fixedLen - 1] \j -> do
                  emit $ "fixed[" <> (show j).t <> "],"
              enumArgs "arg" n
            emit ";"
        -- TODO: don't panic here
        panicBranch

    emit $ "else if (arity < fixed.len + " <> (show n).t <> ")"
    braces do
      emit $ "var args: [" <> (show maxCallSize).t <> "]Box = undefined;"
      emit "for (0..fixed.len) |i|"
      braces do emit "args[i] = fixed[i];"
      for_ [0 :: Int .. n - 1] \i -> do
        emit $ "args[fixed.len + " <> (show i).t <> "] = arg" <> (show i).t <> ";"
      emit "const res = call_closure_boxed_n(pap.closure, args[0..arity]);"
      emit "return apply_boxed_n(res.as_object(), args[arity..]);"

    emit "else"
    braces do
      emit $ "const newPap = Object.allocPap(pap.closure, arity, @intCast(fixed.len + " <> (show n).t <> "));"
      emit "for (0..fixed.len) |i|"
      braces do
        emit "newPap.fixed()[i] = fixed[i];"
      for_ [0 :: Int .. n - 1] \i -> do
        emit $ "newPap.fixed()[fixed.len + " <> (show i).t <> "] = arg" <> (show i).t <> ";"
      emit "return Object.castFrom(newPap).box();"

genApplyBoxed :: Int -> M ()
genApplyBoxed n = do
  emit $ "pub fn apply_boxed_" <> (show n).t
  parens do
    emit "f: *Object,"
    enumParams "arg" n
  emit "Box"
  braces do
    emit "switch (f.tag)"
    braces do
      emit ".Fun =>"
      emit $ "return apply_closure_boxed_" <> (show n).t
      parens do
        emit "f.cast(Closure),"
        enumArgs "arg" n
      emit ","

      emit ".Pap =>"
      emit $ "return apply_pap_boxed_" <> (show n).t
      parens do
        emit "f.cast(Pap),"
        enumArgs "arg" n
      emit ","

genApplyPapBoxed :: Int -> Text
genApplyPapBoxed _ = ""

type M = Writer [Text]

runEmit :: M a -> Text
runEmit = T.intercalate "\n" . snd . runWriter

emit :: Text -> M ()
emit t = tell [t]

delim :: Text -> Text -> M a -> M a
delim d d' m = do
  emit d
  x <- m
  emit d'
  pure x

parens :: M a -> M a
parens = delim "(" ")"

braces :: M a -> M a
braces = delim "{" "}"

commaI :: Int -> Int -> Text
commaI i n = if i == n then "" else ","

commaList :: [a] -> (a -> M b) -> M ()
commaList ts f = for_ (zip [0 ..] ts) \(i, t) -> do
  void $ f t
  when (i < len - 1) do
    emit ","
  where
    len = length ts

panicBranch :: M ()
panicBranch = do
  emit "else => @panic(\"unhandled branch\"),"

branch :: Text -> M b -> M b
branch t act = do
  emit t
  emit "=>"
  x <- braces act
  emit ","
  pure x

genApplyClosureBoxed :: Int -> M ()
genApplyClosureBoxed n = do
  emit $ "pub inline fn apply_closure_boxed_" <> (show n).t
  parens do
    emit "f: *Closure,"
    enumParams "arg" n
  emit "Box"
  braces do
    emit "const arity = f.arity();"

    emit "if"
    parens do
      emit (show n).t
      emit "== arity"
    braces do
      emit $ "return call_closure_boxed_" <> (show n).t
      parens do
        emit "f,"
        enumArgs "arg" n
      emit ";"

    when (n > 1) do
      emit $ "if (" <> (show n).t <> " > arity)"
      braces do
        emit "switch (arity)"
        braces do
          for_ [1 :: Int .. n - 1] \i -> do
            branch (show i).t do
              emit $ "const res = call_closure_boxed_" <> (show i).t
              parens do
                emit "f, "
                enumArgs "arg" i
              emit ";"
              let rest = n - i
              emit $ "return apply_boxed_" <> (show rest).t
              parens do
                emit "res.as_object(),"
                enumArgsFrom "arg" i (i + rest)
              emit ";"
          panicBranch
          -- branch "else" do
          --   emit "var args: [256]Box = undefined;"
          --   emit "for (0..fixed.len) |i|"
          --   braces do
          --     emit "args[i] = fixed[i];"
          --   for_ [0 :: Int .. n - 1] \i -> do
          --     emit $ "args[fixed.len + " <> (show i).t <> "]" <> " = " <> "arg" <> (show i).t <> ";"
          --   emit $ "debug.assert(f.arity() > " <> (show maxCallSize).t <> ");"
          --   emit "const code: *FnBoxedN = @ptrCast(f.code);"
          --   emit "return code(f, &args);"

    emit $ "const pap =  Object.allocPap(f, arity," <> (show n).t <> ");"
    emit "const fixed = pap.fixed();"
    for_ [0 :: Int .. n - 1] \i -> do
      emit $ "fixed[" <> (show i).t <> "] = arg" <> (show i).t <> ";"
    emit "return Object.castFrom(pap).box();"
