#![feature(test)]

///! Test evaluating with scope
extern crate test;

use rhai::{module_resolvers::StaticModuleResolver, Engine, Module, OptimizationLevel};
use test::Bencher;

#[bench]
fn bench_eval_module_merged(bench: &mut Bencher) {
    let script = r#"
        fn foo(x) { x + 1 }
        fn bar(x) { foo(x) }
    "#;

    let mut engine = Engine::new();
    engine.set_optimization_level(OptimizationLevel::None);

    let ast = engine.compile(script).unwrap();

    let module = Module::eval_ast_as_new(Default::default(), &ast, true, &engine).unwrap();

    let mut resolver = StaticModuleResolver::new();
    resolver.insert("testing", module);
    engine.set_module_resolver(Some(resolver));

    let ast = engine
        .compile(
            r#"
                fn foo(x) { x - 1 }
                import "testing" as t;
                t::bar(41)
    "#,
        )
        .unwrap();

    bench.iter(|| engine.consume_ast(&ast).unwrap());
}

#[bench]
fn bench_eval_module_unmerged(bench: &mut Bencher) {
    let script = r#"
        fn foo(x) { x + 1 }
        fn bar(x) { foo(x) }
    "#;

    let mut engine = Engine::new();
    engine.set_optimization_level(OptimizationLevel::None);

    let ast = engine.compile(script).unwrap();

    let module = Module::eval_ast_as_new(Default::default(), &ast, false, &engine).unwrap();

    let mut resolver = StaticModuleResolver::new();
    resolver.insert("testing", module);
    engine.set_module_resolver(Some(resolver));

    let ast = engine
        .compile(
            r#"
                fn foo(x) { x - 1 }
                import "testing" as t;
                t::bar(41)
    "#,
        )
        .unwrap();

    bench.iter(|| engine.consume_ast(&ast).unwrap());
}

#[bench]
fn bench_eval_function_call(bench: &mut Bencher) {
    let mut engine = Engine::new();
    engine.set_optimization_level(OptimizationLevel::None);

    let ast = engine
        .compile(
            r#"
                fn foo(x) { x - 1 }
                fn bar(x) { foo(x) }
                bar(41)
    "#,
        )
        .unwrap();

    bench.iter(|| engine.consume_ast(&ast).unwrap());
}
