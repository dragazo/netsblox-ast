use crate::*;
use std::prelude::v1::*;

#[test]
fn test_opt_cmp() {
    let script = format!(include_str!("script-template.xml"),
        globals = "", fields = "",
        funcs = "", methods = "",
        scripts = r#"<script x="0" y="0"><block s="doDeclareVariables"><list><l>temp</l></list></block><block s="doSetVar"><l>temp</l><block s="reportEquals"><block var="temp"/><block var="temp"/></block></block><block s="doSetVar"><l>temp</l><block s="reportNot"><block s="reportEquals"><block var="temp"/><block var="temp"/></block></block></block><block s="doSetVar"><l>temp</l><block s="reportLessThan"><block var="temp"/><block var="temp"/></block></block><block s="doSetVar"><l>temp</l><block s="reportNot"><block s="reportLessThan"><block var="temp"/><block var="temp"/></block></block></block><block s="doSetVar"><l>temp</l><block s="reportGreaterThan"><block var="temp"/><block var="temp"/></block></block><block s="doSetVar"><l>temp</l><block s="reportNot"><block s="reportGreaterThan"><block var="temp"/><block var="temp"/></block></block></block></script>"#,
    );
    let parser = Parser::builder().omit_nonhat_scripts(false).build().unwrap();
    let ast = parser.parse(&script).unwrap();
    let stmts = &ast.roles[0].entities[0].scripts[0].stmts;
    assert_eq!(stmts.len(), 7);
    match &stmts[1].kind {
        StmtKind::Assign { value, .. } => match &value.kind {
            ExprKind::Eq { .. } => (),
            x => panic!("{:#?}", x),
        }
        x => panic!("{:#?}", x),
    }
}

#[test]
fn test_local_dupe() {
    let script = format!(include_str!("script-template.xml"),
        globals = "", fields = "",
        funcs = r#"<block-definition s="main" type="reporter" category="custom"><header></header><code></code><translations></translations><inputs></inputs><script><block s="doDeclareVariables"><list><l>vals</l></list></block><block s="doSetVar"><l>vals</l><block s="reportNewList"><list></list></block></block><block s="doRepeat"><l>-5</l><script><block s="doAddToList"><l>thing</l><block var="vals"/></block></script></block><block s="doRepeat"><l>2</l><script><block s="doAddToList"><block s="reportNumbers"><l>1</l><l>10</l></block><block var="vals"/></block></script></block><block s="doRepeat"><l>0</l><script><block s="doAddToList"><l>thing</l><block var="vals"/></block></script></block><block s="doFor"><l>i</l><l>1</l><l>10</l><script><block s="doAddToList"><block s="reportNumbers"><block var="i"/><l>7.75</l></block><block var="vals"/></block></script></block><block s="doFor"><l>i</l><l>10</l><l>1</l><script><block s="doAddToList"><block s="reportNumbers"><l>6.5</l><block var="i"/></block><block var="vals"/></block></script></block><block s="doReport"><block var="vals"/></block></script></block-definition>"#,
        methods = "",
        scripts = "",
    );
    let parser = Parser::builder().omit_nonhat_scripts(false).build().unwrap();
    parser.parse(&script).unwrap();
}

#[test]
fn test_lambdas_no_captures_no_inputs() {
    let script = format!(include_str!("script-template.xml"),
        globals = "", fields = "",
        funcs = "", methods = "",
        scripts = r#"<script x="75" y="89.02380952380952"><block s="doDeclareVariables"><list><l>a</l><l>b</l></list></block><block s="doSetVar"><l>a</l><block s="reifyReporter"><autolambda><block s="reportSum"><l>2</l><l>3</l></block></autolambda><list></list></block></block><block s="doSetVar"><l>b</l><block s="reifyScript"><script><block s="doDeclareVariables"><list><l>temp</l><l>b</l></list></block><block s="doSetVar"><l>temp</l><l>67</l></block></script><list></list></block></block></script>"#,
    );
    let parser = Parser::builder().omit_nonhat_scripts(false).build().unwrap();
    let ast = parser.parse(&script).unwrap();
    let stmts = &ast.roles[0].entities[0].scripts[0].stmts;
    assert_eq!(stmts.len(), 3);
    match &stmts[1].kind {
        StmtKind::Assign { value: Expr { kind: ExprKind::Closure { params, captures, stmts }, .. }, .. } => {
            assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), Vec::<&str>::new());
            assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), Vec::<&str>::new());
            assert_eq!(stmts.len(), 1);
            match &stmts[0].kind {
                StmtKind::Return { value: Expr { kind: ExprKind::Add { .. }, .. } } => (),
                x => panic!("{:?}", x),
            }
        }
        x => panic!("{:?}", x),
    }
    match &stmts[2].kind {
        StmtKind::Assign { value: Expr { kind: ExprKind::Closure { params, captures, stmts }, .. }, .. } => {
            assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), Vec::<&str>::new());
            assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), Vec::<&str>::new());
            assert_eq!(stmts.len(), 2);
        }
        x => panic!("{:?}", x),
    }
}

#[test]
fn test_lambdas_no_captures() {
    let script = format!(include_str!("script-template.xml"),
        globals = "", fields = "",
        funcs = "", methods = "",
        scripts = r##"<script x="75" y="89.02380952380952"><block s="doDeclareVariables"><list><l>a</l><l>b</l></list></block><block s="doSetVar"><l>a</l><block s="reifyReporter"><autolambda><block s="reportSum"><block var="#1"/><block var="#2"/></block></autolambda><list><l>#1</l><l>#2</l></list></block></block><block s="doSetVar"><l>b</l><block s="reifyScript"><script><block s="doDeclareVariables"><list><l>temp</l><l>b</l></list></block><block s="doSetVar"><l>temp</l><block var="ght"/></block><block s="doSetVar"><l>b</l><block s="reportPower"><block var="temp"/><block var="brg"/></block></block></script><list><l>ght</l><l>brg</l></list></block></block></script>"##,
    );
    let parser = Parser::builder().omit_nonhat_scripts(false).build().unwrap();
    let ast = parser.parse(&script).unwrap();
    let stmts = &ast.roles[0].entities[0].scripts[0].stmts;
    assert_eq!(stmts.len(), 3);
    match &stmts[1].kind {
        StmtKind::Assign { value: Expr { kind: ExprKind::Closure { params, captures, stmts }, .. }, .. } => {
            assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), vec!["#1", "#2"]);
            assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), Vec::<&str>::new());
            assert_eq!(stmts.len(), 1);
            match &stmts[0].kind {
                StmtKind::Return { value: Expr { kind: ExprKind::Add { .. }, .. } } => (),
                x => panic!("{:?}", x),
            }
        }
        x => panic!("{:?}", x),
    }
    match &stmts[2].kind {
        StmtKind::Assign { value: Expr { kind: ExprKind::Closure { params, captures, stmts }, .. }, .. } => {
            assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), vec!["ght", "brg"]);
            assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), Vec::<&str>::new());
            assert_eq!(stmts.len(), 3);
        }
        x => panic!("{:?}", x),
    }
}

#[test]
fn test_lambdas_adv_1() {
    let script = format!(include_str!("script-template.xml"),
        globals = "", fields = "",
        funcs = "", methods = "",
        scripts = r##"<script x="75" y="89.02380952380952"><block s="doDeclareVariables"><list><l>a</l><l>b</l></list></block><block s="doSetVar"><l>a</l><block s="reifyReporter"><autolambda><block s="reportSum"><block var="a"/><block var="b"/></block></autolambda><list></list></block></block><block s="doSetVar"><l>b</l><block s="reifyScript"><script><block s="doDeclareVariables"><list><l>temp</l></list></block><block s="doSetVar"><l>temp</l><block var="ght"/></block><block s="doSetVar"><l>b</l><block s="reportPower"><block var="temp"/><block var="brg"/></block></block></script><list><l>ght</l><l>brg</l></list></block></block></script>"##,
    );
    let parser = Parser::builder().omit_nonhat_scripts(false).build().unwrap();
    let ast = parser.parse(&script).unwrap();
    let stmts = &ast.roles[0].entities[0].scripts[0].stmts;
    assert_eq!(stmts.len(), 3);
    match &stmts[1].kind {
        StmtKind::Assign { value: Expr { kind: ExprKind::Closure { params, captures, stmts }, .. }, .. } => {
            assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), Vec::<&str>::new());
            assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), vec!["a", "b"]);
            assert_eq!(stmts.len(), 1);
            match &stmts[0].kind {
                StmtKind::Return { value: Expr { kind: ExprKind::Add { .. }, .. }, .. } => (),
                x => panic!("{:?}", x),
            }
        }
        x => panic!("{:?}", x),
    }
    match &stmts[2].kind {
        StmtKind::Assign { value: Expr { kind: ExprKind::Closure { params, captures, stmts }, .. }, .. } => {
            assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), vec!["ght", "brg"]);
            assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), vec!["b"]);
            assert_eq!(stmts.len(), 3);
        }
        x => panic!("{:?}", x),
    }
}

#[test]
fn test_lambdas_adv_2_rep_captures() {
    let script = format!(include_str!("script-template.xml"),
        globals = "", fields = "",
        funcs = "", methods = "",
        scripts = r##"<script x="75" y="89.02380952380952"><block s="doDeclareVariables"><list><l>a</l><l>b</l></list></block><block s="doSetVar"><l>a</l><block s="reifyReporter"><autolambda><block s="reportDifference"><block s="reportProduct"><block s="reportSum"><block var="a"/><block var="b"/></block><block var="b"/></block><block var="a"/></block></autolambda><list></list></block></block><block s="doSetVar"><l>b</l><block s="reifyScript"><script><block s="doDeclareVariables"><list><l>temp</l></list></block><block s="doSetVar"><l>temp</l><block var="ght"/></block><block s="doSetVar"><l>b</l><block s="reportPower"><block var="temp"/><block var="brg"/></block></block><block s="doSetVar"><l>b</l><block s="reportLessThan"><block var="temp"/><block var="b"/></block></block></script><list><l>ght</l><l>brg</l></list></block></block></script>"##,
    );
    let parser = Parser::builder().omit_nonhat_scripts(false).build().unwrap();
    let ast = parser.parse(&script).unwrap();
    let stmts = &ast.roles[0].entities[0].scripts[0].stmts;
    assert_eq!(stmts.len(), 3);
    match &stmts[1].kind {
        StmtKind::Assign { value: Expr { kind: ExprKind::Closure { params, captures, stmts }, .. }, .. } => {
            assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), Vec::<&str>::new());
            assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), vec!["a", "b"]);
            assert_eq!(stmts.len(), 1);
            match &stmts[0].kind {
                StmtKind::Return { .. } => (),
                x => panic!("{:?}", x),
            }
        }
        x => panic!("{:?}", x),
    }
    match &stmts[2].kind {
        StmtKind::Assign { value: Expr { kind: ExprKind::Closure { params, captures, stmts }, .. }, .. } => {
            assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), vec!["ght", "brg"]);
            assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), vec!["b"]);
            assert_eq!(stmts.len(), 4);
        }
        x => panic!("{:?}", x),
    }
}

#[test]
fn test_lambdas_adv_3_nested_captures() {
    let script = format!(include_str!("script-template.xml"),
        globals = "", fields = "",
        funcs = "", methods = "",
        scripts = r##"<script x="75" y="89.02380952380952"><block s="doDeclareVariables"><list><l>a</l><l>b</l></list></block><block s="doSetVar"><l>a</l><block s="reifyReporter"><autolambda><block s="reportDifference"><block s="reportProduct"><block s="reportSum"><block var="a"/><block var="b"/></block><block var="foo"/></block><block var="a"/></block></autolambda><list><l>foo</l></list></block></block><block s="doSetVar"><l>b</l><block s="reifyScript"><script><block s="doDeclareVariables"><list><l>temp</l></list></block><block s="doSetVar"><l>temp</l><block s="reifyReporter"><autolambda><block s="reportPower"><block var="b"/><block s="reportSum"><block var="temp"/><block var="brg"/></block></block></autolambda><list></list></block></block></script><list><l>ght</l><l>brg</l></list></block></block></script>"##,
    );
    let parser = Parser::builder().omit_nonhat_scripts(false).build().unwrap();
    let ast = parser.parse(&script).unwrap();
    let stmts = &ast.roles[0].entities[0].scripts[0].stmts;
    assert_eq!(stmts.len(), 3);
    match &stmts[1].kind {
        StmtKind::Assign { value: Expr { kind: ExprKind::Closure { params, captures, stmts }, .. }, .. } => {
            assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), vec!["foo"]);
            assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), vec!["a", "b"]);
            assert_eq!(stmts.len(), 1);
            match &stmts[0].kind {
                StmtKind::Return { .. } => (),
                x => panic!("{:?}", x),
            }
        }
        x => panic!("{:?}", x),
    }
    match &stmts[2].kind {
        StmtKind::Assign { value: Expr { kind: ExprKind::Closure { params, captures, stmts }, .. }, .. } => {
            assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), vec!["ght", "brg"]);
            assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), vec!["b"]);
            assert_eq!(stmts.len(), 2);
            match &stmts[1].kind {
                StmtKind::Assign { value: Expr { kind: ExprKind::Closure { params, captures, stmts }, .. }, .. } => {
                    assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), Vec::<&str>::new());
                    assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), vec!["b", "temp", "brg"]);
                    assert_eq!(stmts.len(), 1);
                    match &stmts[0].kind {
                        StmtKind::Return { .. } => (),
                        x => panic!("{:?}", x),
                    }
                }
                x => panic!("{:?}", x),
            }
        }
        x => panic!("{:?}", x),
    }
}

#[test]
fn test_run_call_lambdas() {
    let script = format!(include_str!("script-template.xml"),
        globals = "", fields = "",
        funcs = "", methods = "",
        scripts = r##"<script x="67.14285714285715" y="77.66666666666701"><block s="doDeclareVariables"><list><l>a</l><l>b</l></list></block><block s="doSetVar"><l>a</l><block s="evaluate"><block s="reifyReporter"><autolambda><block s="reportSum"><l>4</l><l>5</l></block></autolambda><list></list></block><list></list></block></block><block s="doSetVar"><l>b</l><block s="evaluate"><block s="reifyReporter"><autolambda><block s="reportModulus"><block s="reportSum"><block var="#3"/><block var="#1"/></block><block var="merp"/></block></autolambda><list><l>#1</l><l>merp</l><l>#3</l></list></block><list><l>6</l><l>1</l><l>4</l></list></block></block><block s="doRun"><block s="reifyScript"><script><block s="doSetVar"><l>a</l><l>7</l></block></script><list></list></block><list></list></block><block s="doRun"><block s="reifyScript"><script><block s="doSetVar"><l>a</l><block s="reportSum"><block s="reportProduct"><block var="val"/><block var="b"/></block><block var="rgt"/></block></block></script><list><l>val</l><l>rgt</l></list></block><list><l>8</l><l>7</l></list></block></script>"##,
    );
    let parser = Parser::builder().omit_nonhat_scripts(false).build().unwrap();
    let ast = parser.parse(&script).unwrap();
    let stmts = &ast.roles[0].entities[0].scripts[0].stmts;
    assert_eq!(stmts.len(), 5);
    match &stmts[1].kind {
        StmtKind::Assign { value: Expr { kind: ExprKind::CallClosure { closure, args }, .. }, .. } => {
            match &closure.kind {
                ExprKind::Closure { params, captures, stmts } => {
                    assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<_>>(), Vec::<&str>::new());
                    assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<_>>(), Vec::<&str>::new());
                    assert_eq!(stmts.len(), 1);
                }
                x => panic!("{:?}", x),
            }
            assert_eq!(args.len(), 0);
        }
        x => panic!("{:?}", x),
    }
    match &stmts[2].kind {
        StmtKind::Assign { value: Expr { kind: ExprKind::CallClosure { closure, args }, .. }, .. } => {
            match &closure.kind {
                ExprKind::Closure { params, captures, stmts } => {
                    assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<_>>(), vec!["#1", "merp", "#3"]);
                    assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<_>>(), Vec::<&str>::new());
                    assert_eq!(stmts.len(), 1);
                }
                x => panic!("{:?}", x),
            }
            assert_eq!(args.len(), 3);
        }
        x => panic!("{:?}", x),
    }
    match &stmts[3].kind {
        StmtKind::RunClosure { closure: Expr { kind: ExprKind::Closure { params, captures, stmts }, .. }, args } => {
            assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<_>>(), Vec::<&str>::new());
            assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<_>>(), vec!["a"]);
            assert_eq!(stmts.len(), 1);
            assert_eq!(args.len(), 0);
        }
        x => panic!("{:?}", x),
    }
    match &stmts[4].kind {
        StmtKind::RunClosure { closure: Expr { kind: ExprKind::Closure { params, captures, stmts }, .. }, args } => {
            assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<_>>(), vec!["val", "rgt"]);
            assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<_>>(), vec!["a", "b"]);
            assert_eq!(stmts.len(), 1);
            assert_eq!(args.len(), 2);
        }
        x => panic!("{:?}", x),
    }
}

#[test]
fn test_auto_fill_lambda_args() {
    let script = format!(include_str!("script-template.xml"),
        globals = "", fields = "",
        funcs = "", methods = "",
        scripts = r##"<script x="58.57142857142858" y="44.571428571428555"><block s="doDeclareVariables"><list><l>a</l></list></block><block s="doSetVar"><l>a</l><block s="reportSum"><l>6</l><l>7</l></block></block><block s="doSetVar"><l>a</l><block s="reportSum"><l>6</l><l></l></block></block><block s="doSetVar"><l>a</l><block s="reportSum"><l></l><l>7</l></block></block><block s="doSetVar"><l>a</l><block s="reportSum"><l></l><l></l></block></block><block s="doSetVar"><l>a</l><block s="reifyReporter"><autolambda><block s="reportSum"><l>6</l><l>7</l></block></autolambda><list></list></block></block><block s="doSetVar"><l>a</l><block s="reifyReporter"><autolambda><block s="reportSum"><l>6</l><l></l></block></autolambda><list></list></block></block><block s="doSetVar"><l>a</l><block s="reifyReporter"><autolambda><block s="reportSum"><l></l><l>7</l></block></autolambda><list></list></block></block><block s="doSetVar"><l>a</l><block s="reifyReporter"><autolambda><block s="reportSum"><l></l><l></l></block></autolambda><list></list></block></block><block s="doSetVar"><l>a</l><block s="reifyReporter"><autolambda><block s="reportSum"><block s="reportSum"><l></l><block s="reifyReporter"><autolambda><block s="reportSum"><l></l><l></l></block></autolambda><list></list></block></block><l></l></block></autolambda><list></list></block></block><block s="doSetVar"><l>a</l><block s="reifyReporter"><autolambda><block s="reportSum"><block s="reportSum"><l></l><block s="reifyReporter"><autolambda><block s="reportSum"><block var="#1"/><l></l></block></autolambda><list><l>#1</l></list></block></block><l></l></block></autolambda><list></list></block></block></script>"##,
    );
    let parser = Parser::builder().omit_nonhat_scripts(false).build().unwrap();
    let ast = parser.parse(&script).unwrap();
    let stmts = &ast.roles[0].entities[0].scripts[0].stmts;
    assert_eq!(stmts.len(), 11);

    for (i, expect_left, expect_right) in [(1, "6", "7"), (2, "6", ""), (3, "", "7"), (4, "", "")] {
        match &stmts[i].kind {
            StmtKind::Assign { value, .. } => match &value.kind {
                ExprKind::Add { values } => {
                    let (left, right) = match values {
                        VariadicInput::Fixed(x) => {
                            assert_eq!(x.len(), 2);
                            (&x[0], &x[1])
                        }
                        x => panic!("{x:?}"),
                    };
                    match &left.kind {
                        ExprKind::Value(Value::String(x)) => assert_eq!(x, expect_left),
                        x => panic!("{x:?}"),
                    }
                    match &right.kind {
                        ExprKind::Value(Value::String(x)) => assert_eq!(x, expect_right),
                        x => panic!("{x:?}"),
                    }
                }
                x => panic!("{x:?}"),
            }
            x => panic!("{x:?}"),
        }
    }

    match &stmts[5].kind {
        StmtKind::Assign { value, .. } => match &value.kind {
            ExprKind::Closure { params, stmts, .. } => {
                assert_eq!(params.len(), 0);
                assert_eq!(stmts.len(), 1);
                match &stmts[0].kind {
                    StmtKind::Return { value } => match &value.kind {
                        ExprKind::Add { values } => {
                            let (left, right) = match values {
                                VariadicInput::Fixed(x) => {
                                    assert_eq!(x.len(), 2);
                                    (&x[0], &x[1])
                                }
                                x => panic!("{x:?}"),
                            };
                            match &left.kind {
                                ExprKind::Value(Value::String(x)) => assert_eq!(x, "6"),
                                x => panic!("{x:?}"),
                            }
                            match &right.kind {
                                ExprKind::Value(Value::String(x)) => assert_eq!(x, "7"),
                                x => panic!("{x:?}"),
                            }
                        }
                        x => panic!("{x:?}"),
                    }
                    x => panic!("{x:?}"),
                }
            }
            x => panic!("{x:?}"),
        }
        x => panic!("{x:?}"),
    }
    match &stmts[6].kind {
        StmtKind::Assign { value, .. } => match &value.kind {
            ExprKind::Closure { params, stmts, .. } => {
                assert_eq!(params.len(), 1);
                assert_eq!(params[0].name, "%1");
                assert_eq!(stmts.len(), 1);
                match &stmts[0].kind {
                    StmtKind::Return { value } => match &value.kind {
                        ExprKind::Add { values } => {
                            let (left, right) = match values {
                                VariadicInput::Fixed(x) => {
                                    assert_eq!(x.len(), 2);
                                    (&x[0], &x[1])
                                }
                                x => panic!("{x:?}"),
                            };
                            match &left.kind {
                                ExprKind::Value(Value::String(x)) => assert_eq!(x, "6"),
                                x => panic!("{x:?}"),
                            }
                            match &right.kind {
                                ExprKind::Variable { var } => assert_eq!(var.name, "%1"),
                                x => panic!("{x:?}"),
                            }
                        }
                        x => panic!("{x:?}"),
                    }
                    x => panic!("{x:?}"),
                }
            }
            x => panic!("{x:?}"),
        }
        x => panic!("{x:?}"),
    }
    match &stmts[7].kind {
        StmtKind::Assign { value, .. } => match &value.kind {
            ExprKind::Closure { params, stmts, .. } => {
                assert_eq!(params.len(), 1);
                assert_eq!(params[0].name, "%1");
                assert_eq!(stmts.len(), 1);
                match &stmts[0].kind {
                    StmtKind::Return { value } => match &value.kind {
                        ExprKind::Add { values } => {
                            let (left, right) = match values {
                                VariadicInput::Fixed(x) => {
                                    assert_eq!(x.len(), 2);
                                    (&x[0], &x[1])
                                }
                                x => panic!("{x:?}"),
                            };
                            match &left.kind {
                                ExprKind::Variable { var } => assert_eq!(var.name, "%1"),
                                x => panic!("{x:?}"),
                            }
                            match &right.kind {
                                ExprKind::Value(Value::String(x)) => assert_eq!(x, "7"),
                                x => panic!("{x:?}"),
                            }
                        }
                        x => panic!("{x:?}"),
                    }
                    x => panic!("{x:?}"),
                }
            }
            x => panic!("{x:?}"),
        }
        x => panic!("{x:?}"),
    }
    match &stmts[8].kind {
        StmtKind::Assign { value, .. } => match &value.kind {
            ExprKind::Closure { params, stmts, .. } => {
                assert_eq!(params.len(), 2);
                assert_eq!(params[0].name, "%1");
                assert_eq!(params[1].name, "%2");
                assert_eq!(stmts.len(), 1);
                match &stmts[0].kind {
                    StmtKind::Return { value } => match &value.kind {
                        ExprKind::Add { values } => {
                            let (left, right) = match values {
                                VariadicInput::Fixed(x) => {
                                    assert_eq!(x.len(), 2);
                                    (&x[0], &x[1])
                                }
                                x => panic!("{x:?}"),
                            };
                            match &left.kind {
                                ExprKind::Variable { var } => assert_eq!(var.name, "%1"),
                                x => panic!("{x:?}"),
                            }
                            match &right.kind {
                                ExprKind::Variable { var } => assert_eq!(var.name, "%2"),
                                x => panic!("{x:?}"),
                            }
                        }
                        x => panic!("{x:?}"),
                    }
                    x => panic!("{x:?}"),
                }
            }
            x => panic!("{x:?}"),
        }
        x => panic!("{x:?}"),
    }
    match &stmts[9].kind {
        StmtKind::Assign { value, .. } => match &value.kind {
            ExprKind::Closure { params, stmts, .. } => {
                assert_eq!(params.len(), 4);
                assert_eq!(params[0].name, "%1");
                assert_eq!(params[1].name, "%2");
                assert_eq!(params[2].name, "%3");
                assert_eq!(params[3].name, "%4");
                assert_eq!(stmts.len(), 1);
                match &stmts[0].kind {
                    StmtKind::Return { value } => match &value.kind {
                        ExprKind::Add { values } => {
                            let (left, right) = match values {
                                VariadicInput::Fixed(x) => {
                                    assert_eq!(x.len(), 2);
                                    (&x[0], &x[1])
                                }
                                x => panic!("{x:?}"),
                            };
                            match &left.kind {
                                ExprKind::Add { values } => {
                                    let (left, right) = match values {
                                        VariadicInput::Fixed(x) => {
                                            assert_eq!(x.len(), 2);
                                            (&x[0], &x[1])
                                        }
                                        x => panic!("{x:?}"),
                                    };
                                    match &left.kind {
                                        ExprKind::Variable { var } => assert_eq!(var.name, "%1"),
                                        x => panic!("{x:?}"),
                                    }
                                    match &right.kind {
                                        ExprKind::Closure { params, stmts, .. } => {
                                            assert_eq!(params.len(), 2);
                                            assert_eq!(params[0].name, "%2");
                                            assert_eq!(params[1].name, "%3");
                                            assert_eq!(stmts.len(), 1);
                                            match &stmts[0].kind {
                                                StmtKind::Return { value } => match &value.kind {
                                                    ExprKind::Add { values } => {
                                                        let (left, right) = match values {
                                                            VariadicInput::Fixed(x) => {
                                                                assert_eq!(x.len(), 2);
                                                                (&x[0], &x[1])
                                                            }
                                                            x => panic!("{x:?}"),
                                                        };
                                                        match &left.kind {
                                                            ExprKind::Variable { var } => assert_eq!(var.name, "%2"),
                                                            x => panic!("{x:?}"),
                                                        }
                                                        match &right.kind {
                                                            ExprKind::Variable { var } => assert_eq!(var.name, "%3"),
                                                            x => panic!("{x:?}"),
                                                        }
                                                    }
                                                    x => panic!("{x:?}"),
                                                }
                                                x => panic!("{x:?}"),
                                            }
                                        }
                                        x => panic!("{x:?}"),
                                    }
                                }
                                x => panic!("{x:?}"),
                            }
                            match &right.kind {
                                ExprKind::Variable { var } => assert_eq!(var.name, "%4"),
                                x => panic!("{x:?}"),
                            }
                        }
                        x => panic!("{x:?}"),
                    }
                    x => panic!("{x:?}"),
                }
            }
            x => panic!("{x:?}"),
        }
        x => panic!("{x:?}"),
    }
    match &stmts[10].kind {
        StmtKind::Assign { value, .. } => match &value.kind {
            ExprKind::Closure { params, stmts, ..} => {
                assert_eq!(params.len(), 2);
                assert_eq!(params[0].name, "%1");
                assert_eq!(params[1].name, "%2");
                assert_eq!(stmts.len(), 1);
                match &stmts[0].kind {
                    StmtKind::Return { value } => match &value.kind {
                        ExprKind::Add { values } => {
                            let (left, right) = match values {
                                VariadicInput::Fixed(x) => {
                                    assert_eq!(x.len(), 2);
                                    (&x[0], &x[1])
                                }
                                x => panic!("{x:?}"),
                            };
                            match &left.kind {
                                ExprKind::Add { values } => {
                                    let (left, right) = match values {
                                        VariadicInput::Fixed(x) => {
                                            assert_eq!(x.len(), 2);
                                            (&x[0], &x[1])
                                        }
                                        x => panic!("{x:?}"),
                                    };
                                    match &left.kind {
                                        ExprKind::Variable { var } => assert_eq!(var.name, "%1"),
                                        x => panic!("{x:?}"),
                                    }
                                    match &right.kind {
                                        ExprKind::Closure { params, stmts, .. } => {
                                            assert_eq!(params.len(), 1);
                                            assert_eq!(params[0].name, "#1");
                                            assert_eq!(stmts.len(), 1);
                                            match &stmts[0].kind {
                                                StmtKind::Return { value } => match &value.kind {
                                                    ExprKind::Add { values } => {
                                                        let (left, right) = match values {
                                                            VariadicInput::Fixed(x) => {
                                                                assert_eq!(x.len(), 2);
                                                                (&x[0], &x[1])
                                                            }
                                                            x => panic!("{x:?}"),
                                                        };
                                                        match &left.kind {
                                                            ExprKind::Variable { var } => assert_eq!(var.name, "#1"),
                                                            x => panic!("{x:?}"),
                                                        }
                                                        match &right.kind {
                                                            ExprKind::Value(Value::String(x)) => assert_eq!(x, ""),
                                                            x => panic!("{x:?}"),
                                                        }
                                                    }
                                                    x => panic!("{x:?}"),
                                                }
                                                x => panic!("{x:?}"),
                                            }
                                        }
                                        x => panic!("{x:?}"),
                                    }
                                }
                                x => panic!("{x:?}"),
                            }
                            match &right.kind {
                                ExprKind::Variable { var } => assert_eq!(var.name, "%2"),
                                x => panic!("{x:?}"),
                            }
                        }
                        x => panic!("{x:?}"),
                    }
                    x => panic!("{x:?}"),
                }
            }
            x => panic!("{x:?}"),
        }
        x => panic!("{x:?}"),
    }
}

#[test]
fn test_export_formats() {
    let parser = Parser::builder().omit_nonhat_scripts(false).build().unwrap();

    parser.parse(include_str!("projects/raw-role-export.xml")).unwrap();
    parser.parse(include_str!("projects/role-export.xml")).unwrap();
    parser.parse(include_str!("projects/project-export.xml")).unwrap();
}
