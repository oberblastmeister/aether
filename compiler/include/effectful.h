#ifndef _EFFECTFUL_H
#define _EFFECTFUL_H

#define state(ty) \
    type State' = State ty; \
    get' :: State' :> es => Eff es ty; \
    get' = get @(ty); \
    gets' :: State' :> es => (ty -> a) -> Eff es a; \
    gets' = gets @(ty); \
    put' :: State' :> es => ty -> Eff es (); \
    put' = put @(ty); \
    modify' :: State' :> es => (ty -> ty) -> Eff es (); \
    modify' = modify @(ty); \
    use' :: (Is k A_Getter, State' :> es) => Optic' k is ty a -> Eff es a; \
    use' o = gets @(ty) (^. o); \
    (.=) :: (Is k A_Setter, State' :> es) => Optic' k is ty a -> a -> Eff es (); \
    o .= s = modify @(ty) (o .~ s); \
    (%=) :: (Is k A_Setter, State' :> es) => Optic' k is ty a -> (a -> a) -> Eff es (); \
    o %= f = modify @(ty) (o %~ f); \
    

#define reader(ty) \
    type Reader' = Reader ty; \
    ask' :: Reader' :> es => Eff es ty; \
    ask' = ask @(ty); \
    asks' :: Reader' :> es => (ty -> a) -> Eff es a; \
    asks' = asks @(ty);

#else
#endif
