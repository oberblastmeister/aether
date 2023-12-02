import os
import subprocess
from dataclasses import dataclass


@dataclass(frozen=True)
class Box:
    pass


@dataclass(frozen=True)
class U64:
    pass


@dataclass(frozen=True)
class D64:
    pass


Rep = Box


def rep_type(rep: Rep) -> str:
    match rep:
        case Box():
            return "Box"


def rep_name(rep: Rep) -> str:
    match rep:
        case Box():
            return "b"


def gen_names(reps: list[Rep]) -> str:
    return "_".join(rep_name(rep) for rep in reps)


def gen_arg_list(prefix: str, args: list[Rep]) -> str:
    n = len(args)
    return ",".join(f"{prefix}{i}: {rep_type(arg)}" for i, arg in enumerate(args))


def gen_call_list(args: list[Rep]) -> str:
    n = len(args)
    return ",".join(f"arg{i}" for i, arg in enumerate(args))


def gen_args(arg_len: int) -> str:
    return ",".join(str(i) for i in range(arg_len))


def gen_fn_ty(args: list[Rep], ret: Rep) -> str:
    return f"fn(f: *Closure, {gen_arg_list('arg', args)}) {rep_type(ret)}"


def gen_args_from_env(env: str, args: list[Rep]) -> str:
    return ",".join(f"{env}[{i + 1}]" for i, arg in enumerate(args))


def gen_allocate_pap_fn(args: list[Rep], rest: list[Rep], ret: Rep) -> str:
    n = len(args)
    return f"""

    pub fn allocate_pap_{gen_names(args + rest)}(f: *Closure, {gen_arg_list('arg', args)}, allocator: Allocator) *Closure {{
        const F = struct {{
            fn pap_call(self: *Closure, {gen_arg_list('rest', rest)}) {{
                const InnerFn = {gen_fn_ty(args + rest, ret)};
                const env = self.env();
                const inner_fn = env[0].as_object().cast(Closure);
                const code: *InnerFn = @ptrCast(inner_fn.code);
                code(f, {gen_args_from_env("env", args)}, {gen_call_list(rest)})
            }}
        }};
        const closure: *Closure = Object.allocClosure({n + 1}, {len(rest)}, allocator);
        const env = closure.env();
        env[0] = f;
        {";".join(f"args[{i}] = arg{i}" for i in range(1, n + 1))};
        return closure;
    }}
    """


def gen_apply_fn(args: list[Rep], ret: Rep) -> str:
    n = len(args)
    assert ret == Box()
    return f"""
    pub fn apply_closure_{n}(f: *Closure, {gen_arg_list("arg", args)}) {rep_type(ret)} {{

    }}
    """


def gen_apply(args: list[Rep]) -> str:
    n = len(args)
    f"""
    from jinja2 import Environment, FileSystemLoader
    env = Environment(loader=FileSystemLoader('./'))
    template = env.get_template(name)
    """
    pass


def generate_file(name, outpath, **kwargs):
    from jinja2 import Environment, FileSystemLoader

    env = Environment(loader=FileSystemLoader("./"))
    template = env.get_template(name)
    path = os.path.join(outpath, name)
    with open(path, "w") as fp:
        fp.write(template.render(kwargs))
    subprocess.run(["zig", "fmt", path])


def main():
    generate_file("apply.zig", "../src/", amount=16)


if __name__ == "__main__":
    main()
