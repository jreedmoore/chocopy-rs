use cc::compiler;

fn assert_output(program: &str, expected_output: Vec<&str>) {
    let ir = compiler::produce_stack_ir(program).unwrap();
    let actual = stack_vm::vm::VM::run_with_mock_io(&ir);

    assert_eq!(actual, expected_output, "in example: {}", program,);
}

#[test]
fn test_exprs() {
    assert_output("print(1 + 2)", vec!["3"]);
    assert_output("print(1 - 2)", vec!["-1"]);
    assert_output("print(-1)", vec!["-1"]);
    assert_output("print(True)", vec!["True"]);
    assert_output("print(not True)", vec!["False"]);
    assert_output("print(False)", vec!["False"]);
    assert_output("print(True or False)", vec!["True"]);
    assert_output("1", vec![]);
    assert_output("print(2 * 2)", vec!["4"]);
    assert_output("print(2 // 2)", vec!["1"]);
    assert_output("print(5 % 2)", vec!["1"]);
    assert_output("print(1 < 2)", vec!["True"]);
    assert_output("print(1 > 2)", vec!["False"]);
    assert_output("print(5 > 0)", vec!["True"]);
    assert_output("print(1 == 2)", vec!["False"]);
    assert_output("print(1 >= 1)", vec!["True"]);
    assert_output("print(1 <= 1)", vec!["True"]);
    assert_output("print((1 > 2) or (1 < 2))", vec!["True"]);
    assert_output("print((1 > 2))\nprint((1 < 2))", vec!["False", "True"]);
    assert_output("print(1 if True else 2)", vec!["1"]);
    assert_output("print(1 if False else 2)", vec!["2"]);
    assert_output("print(None is None)", vec!["True"]);
}

#[test]
fn test_locals() {
    assert_output("x: int = 1\nprint(x)", vec!["1"]);
    assert_output("x: int = 1\nprint(x)", vec!["1"]);
    assert_output("x: bool = False\nprint(x)", vec!["False"]);
}

#[test]
fn test_if() {
    assert_output("if True:\n  print(1)", vec!["1"]);
    assert_output("if False:\n  print(1)\nelse:\n  print(2)", vec!["2"]);
    assert_output(
        "if False:\n  print(1)\nelif True:\n  print(3)\nelse:\n  print(2)",
        vec!["3"],
    );
}

#[test]
fn test_loop() {
    assert_output(
        "x: int = 5\nwhile x > 0:\n  print(x)\n  x = x - 1",
        vec!["5", "4", "3", "2", "1"],
    );
}

#[test]
fn test_strings() {
    assert_output("print(\"hello world\")", vec!["hello world"]);
    assert_output("print(\"hello \" + \"world\")", vec!["hello world"]);
    assert_output("x: str = \"abc\"\nprint(x)", vec!["abc"]);
    assert_output("x: str = \"abc\"\nprint(x[1])", vec!["b"]);
    assert_output("print(\"abc\" == \"abc\")", vec!["True"]);
    assert_output("print(\"abc\" is \"abc\")", vec!["False"]);
    assert_output("print(len(\"abc\"))", vec!["3"]);
}

#[test]
fn test_lists() {
    assert_output("l: [int] = [1,2,3]\nprint(l[2])", vec!["3"]);
    assert_output("l: [int] = [1,2,3]\nprint(len(l))", vec!["3"]);
    assert_output("a: [int] = [1,2]\nb: [int] = [3]\na = a + b\nprint(a[2])", vec!["3"]);
    assert_output("l: [int] = [1,2,3]\nl[2] = 4\nprint(l[2])", vec!["4"]);
    assert_output("l: [int] = [1,2,3]\nfor i in l:\n  print(i)", vec!["1", "2", "3"]);
    // need assignability analysis for a test like `c: [int] = []\nc = a + b`
}

#[test]
fn test_functions() {
    assert_output("def f(x: int) -> int:\n  return x + 1\nprint(f(1))", vec!["2"]);
    assert_output(
        "def f(x: int) -> int:\n  return x + 1\ndef g(y: int) -> int:\n  return f(y * 2)\nprint(g(1))",
        vec!["3"],
    );
    assert_output("def p(x: int):\n  print(x)\n  return\np(1)", vec!["1"]);
    assert_output("def f():\n  pass\ndef g():\n  print(1)\nf()", vec![]);
}

fn with_preamble(prog: &str) -> String {
    let preamble = r#"
def t() -> bool:
    print("t")
    return True

def f() -> bool:
    print("f")
    return False

"#;

    preamble.to_owned() + prog
}

#[test]
fn test_short_circuiting() {
    assert_output(&with_preamble("t() or True"), vec!["t"]);
    assert_output(&with_preamble("True or t()"), vec![]);
    assert_output(&with_preamble("f() and False"), vec!["f"]);
    assert_output(&with_preamble("False and f()"), vec![]);
}

#[test]
fn test_classes() {
    let method = r#"
class A(object):
    def a(self: "A"):
        print("a")

x: A = None
x = A()
x.a()
    "#;

    assert_output(method, vec!["a"]);

    let var = r#"
class B(object):
    b: int = 1
    
x: B = None
x = B()
print(x.b)
# x.b = 2
print(x.b)
    "#;

    //TODO: implement class member var store
    assert_output(var, vec!["1", "2"]);
}