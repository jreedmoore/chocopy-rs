use cc::compiler;

fn assert_fails_static_checks(reason: &str, program: &str) {
    let res = compiler::produce_stack_ir(program);

    match res {
        Ok(_) => panic!("Expected failure with reason {}, got success for\n{}", reason, program),
        Err(e) => assert!(e.to_string().contains(reason), "Expected failure with reason {} got {} for\n{}", reason, e, program),
    }
}

fn assert_compiles(program: &str) {
    let res = compiler::produce_stack_ir(program);

    match res {
        Ok(_) => (),
        Err(e) => panic!("Expected successful compile, got error {} for\n{}", e, program),
    }
}

#[test]
fn test_return_analysis() {
    assert_fails_static_checks("FunctionWithoutReturn", "def f() -> int:\n  pass");
    assert_fails_static_checks("FunctionWithoutReturn", "def f() -> int:\n  if True:\n    return 1");
    assert_fails_static_checks("TypeMismatch", "def f() -> int:\n  return False");
    assert_fails_static_checks("TypeMismatch", "def f():\n  return 1");
    assert_compiles("def f():\n  pass");
    assert_compiles("def f():\n  if True:\n    return\n  else:\n    pass");
    assert_compiles("def f() -> int:\n  if True:\n    return 1\n  else:\n    return 2");
}

#[test]
fn test_function_call() {
    assert_fails_static_checks("NotBound", "f()");
    assert_fails_static_checks("TypeMismatch", "len(1)");
}