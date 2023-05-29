use monty::{Executor, Exit};
use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct Case {
    code: String,
    expected: String,
}

#[derive(Debug, Deserialize)]
struct Cases {
    cases: Vec<Case>,
}

#[test]
fn test_return() {
    let cases: Cases = toml::from_str(include_str!("return-cases.toml")).unwrap();
    // dbg!(&cases.cases);
    for case in cases.cases {
        let ex = Executor::new(&case.code, "test.py", &[]).unwrap();
        let output = match ex.run(vec![]) {
            Ok(Exit::Return(value)) => format!("{:?}", value),
            otherwise => panic!("Unexpected exit: {:?}", otherwise),
        };
        let expected = case.expected.trim_matches('\n');
        assert_eq!(output, expected);
    }
}
