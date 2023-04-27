use cc::compiler;

fn assert_fails_static_checks(program: &str) {
    let res = compiler::produce_stack_ir(program);

    assert!(res.is_err())
}

#[test]
fn test_return_analysis() {
    assert_fails_static_checks("def f() -> int:\n  pass")
}