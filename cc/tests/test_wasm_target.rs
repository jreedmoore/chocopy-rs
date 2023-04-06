use cc::compiler;
use cc::runtime;

fn assert_output(program: &str, expected_output: Vec<&str>) -> anyhow::Result<()> {
    let wat = compiler::produce_wat(program)?;
    let actual = runtime::run_with_mocked_io(&wat, &vec![])?;

    assert_eq!(expected_output, actual, "in example: {}", program);

    Ok(())
}

#[test]
fn test_exprs() -> anyhow::Result<()> {
    assert_output("print(1 + 2)", vec!["3"])?;
    assert_output("print(1 - 2)", vec!["-1"])?;
    assert_output("print(-1)", vec!["-1"])?;
    assert_output("print(True)", vec!["True"])?;
    assert_output("print(not True)", vec!["False"])?;
    assert_output("print(False)", vec!["False"])?;
    assert_output("print(True or False)", vec!["True"])?;
    assert_output("1", vec![])?;
    assert_output("print(2 * 2)", vec!["4"])?;
    assert_output("print(2 // 2)", vec!["1"])?;
    assert_output("print(5 % 2)", vec!["1"])?;
    assert_output("print(1 < 2)", vec!["True"])?;
    assert_output("print(1 > 2)", vec!["False"])?;
    assert_output("print(1 == 2)", vec!["False"])?;
    assert_output("print(1 >= 1)", vec!["True"])?;
    assert_output("print(1 <= 1)", vec!["True"])?;
    assert_output("print((1 > 2) or (1 < 2))", vec!["True"])?;
    assert_output("print((1 > 2))\nprint((1 < 2))", vec!["False", "True"])?;
    Ok(())
}