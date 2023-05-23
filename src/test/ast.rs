use crate::*;
use std::prelude::v1::*;

#[test]
fn test_opt_cmp() {
    let script = format!(include_str!("script-template.xml"),
        globals = "", fields = "",
        funcs = "", methods = "",
        scripts = r#"<script x="0" y="0"><block s="doDeclareVariables"><list><l>temp</l></list></block><block s="doSetVar"><l>temp</l><block s="reportEquals"><block var="temp"/><block var="temp"/></block></block><block s="doSetVar"><l>temp</l><block s="reportNot"><block s="reportEquals"><block var="temp"/><block var="temp"/></block></block></block><block s="doSetVar"><l>temp</l><block s="reportLessThan"><block var="temp"/><block var="temp"/></block></block><block s="doSetVar"><l>temp</l><block s="reportNot"><block s="reportLessThan"><block var="temp"/><block var="temp"/></block></block></block><block s="doSetVar"><l>temp</l><block s="reportGreaterThan"><block var="temp"/><block var="temp"/></block></block><block s="doSetVar"><l>temp</l><block s="reportNot"><block s="reportGreaterThan"><block var="temp"/><block var="temp"/></block></block></block></script>"#,
    );
    let parser = Parser { omit_nonhat_scripts: false, ..Default::default() };
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
    let parser = Parser { omit_nonhat_scripts: false, ..Default::default() };
    parser.parse(&script).unwrap();
}

#[test]
fn test_lambdas_no_captures_no_inputs() {
    let script = format!(include_str!("script-template.xml"),
        globals = "", fields = "",
        funcs = "", methods = "",
        scripts = r#"<script x="75" y="89.02380952380952"><block s="doDeclareVariables"><list><l>a</l><l>b</l></list></block><block s="doSetVar"><l>a</l><block s="reifyReporter"><autolambda><block s="reportSum"><l>2</l><l>3</l></block></autolambda><list></list></block></block><block s="doSetVar"><l>b</l><block s="reifyScript"><script><block s="doDeclareVariables"><list><l>temp</l><l>b</l></list></block><block s="doSetVar"><l>temp</l><l>67</l></block></script><list></list></block></block></script>"#,
    );
    let parser = Parser { omit_nonhat_scripts: false, ..Default::default() };
    let ast = parser.parse(&script).unwrap();
    let stmts = &ast.roles[0].entities[0].scripts[0].stmts;
    assert_eq!(stmts.len(), 3);
    match &stmts[1].kind {
        StmtKind::Assign { value, .. } => match &**value {
            Expr { kind: ExprKind::Closure { kind: _, params, captures, stmts }, .. } => {
                assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), Vec::<&str>::new());
                assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), Vec::<&str>::new());
                assert_eq!(stmts.len(), 1);
                match &stmts[0].kind {
                    StmtKind::Return { value } => match &**value {
                        Expr { kind: ExprKind::Add { .. }, .. } => (),
                        x => panic!("{x:?}"),
                    }
                    x => panic!("{x:?}"),
                }
            }
            x => panic!("{x:?}"),
        }
        x => panic!("{x:?}"),
    }
    match &stmts[2].kind {
        StmtKind::Assign { value, .. } => match &**value {
            Expr { kind: ExprKind::Closure { kind: _, params, captures, stmts }, .. } => {
                assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), Vec::<&str>::new());
                assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), Vec::<&str>::new());
                assert_eq!(stmts.len(), 2);
            }
            x => panic!("{x:?}"),
        }
        x => panic!("{x:?}"),
    }
}

#[test]
fn test_lambdas_no_captures() {
    let script = format!(include_str!("script-template.xml"),
        globals = "", fields = "",
        funcs = "", methods = "",
        scripts = r##"<script x="75" y="89.02380952380952"><block s="doDeclareVariables"><list><l>a</l><l>b</l></list></block><block s="doSetVar"><l>a</l><block s="reifyReporter"><autolambda><block s="reportSum"><block var="#1"/><block var="#2"/></block></autolambda><list><l>#1</l><l>#2</l></list></block></block><block s="doSetVar"><l>b</l><block s="reifyScript"><script><block s="doDeclareVariables"><list><l>temp</l><l>b</l></list></block><block s="doSetVar"><l>temp</l><block var="ght"/></block><block s="doSetVar"><l>b</l><block s="reportPower"><block var="temp"/><block var="brg"/></block></block></script><list><l>ght</l><l>brg</l></list></block></block></script>"##,
    );
    let parser = Parser { omit_nonhat_scripts: false, ..Default::default() };
    let ast = parser.parse(&script).unwrap();
    let stmts = &ast.roles[0].entities[0].scripts[0].stmts;
    assert_eq!(stmts.len(), 3);
    match &stmts[1].kind {
        StmtKind::Assign { value, .. } => match &**value {
            Expr { kind: ExprKind::Closure { kind: _, params, captures, stmts }, .. } => {
                assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), vec!["#1", "#2"]);
                assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), Vec::<&str>::new());
                assert_eq!(stmts.len(), 1);
                match &stmts[0].kind {
                    StmtKind::Return { value } => match &**value {
                        Expr { kind: ExprKind::Add { .. }, .. } => (),
                        x => panic!("{x:?}"),
                    },
                    x => panic!("{x:?}"),
                }
            }
            x => panic!("{x:?}"),
        }
        x => panic!("{x:?}"),
    }
    match &stmts[2].kind {
        StmtKind::Assign { value, .. } => match &**value {
            Expr { kind: ExprKind::Closure { kind: _, params, captures, stmts }, .. } => {
                assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), vec!["ght", "brg"]);
                assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), Vec::<&str>::new());
                assert_eq!(stmts.len(), 3);
            }
            x => panic!("{x:?}"),
        }
        x => panic!("{x:?}"),
    }
}

#[test]
fn test_lambdas_adv_1() {
    let script = format!(include_str!("script-template.xml"),
        globals = "", fields = "",
        funcs = "", methods = "",
        scripts = r##"<script x="75" y="89.02380952380952"><block s="doDeclareVariables"><list><l>a</l><l>b</l></list></block><block s="doSetVar"><l>a</l><block s="reifyReporter"><autolambda><block s="reportSum"><block var="a"/><block var="b"/></block></autolambda><list></list></block></block><block s="doSetVar"><l>b</l><block s="reifyScript"><script><block s="doDeclareVariables"><list><l>temp</l></list></block><block s="doSetVar"><l>temp</l><block var="ght"/></block><block s="doSetVar"><l>b</l><block s="reportPower"><block var="temp"/><block var="brg"/></block></block></script><list><l>ght</l><l>brg</l></list></block></block></script>"##,
    );
    let parser = Parser { omit_nonhat_scripts: false, ..Default::default() };
    let ast = parser.parse(&script).unwrap();
    let stmts = &ast.roles[0].entities[0].scripts[0].stmts;
    assert_eq!(stmts.len(), 3);
    match &stmts[1].kind {
        StmtKind::Assign { value, .. } => match &**value {
            Expr { kind: ExprKind::Closure { kind: _, params, captures, stmts }, .. } => {
                assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), Vec::<&str>::new());
                assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), vec!["a", "b"]);
                assert_eq!(stmts.len(), 1);
                match &stmts[0].kind {
                    StmtKind::Return { value, .. } => match &**value {
                        Expr { kind: ExprKind::Add { .. }, .. } => (),
                        x => panic!("{x:?}"),
                    }
                    x => panic!("{x:?}"),
                }
            }
            x => panic!("{x:?}"),
        }
        x => panic!("{x:?}"),
    }
    match &stmts[2].kind {
        StmtKind::Assign { value, .. } => match &**value {
            Expr { kind: ExprKind::Closure { kind: _, params, captures, stmts }, .. } => {
                assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), vec!["ght", "brg"]);
                assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), vec!["b"]);
                assert_eq!(stmts.len(), 3);
            }
            x => panic!("{x:?}"),
        }
        x => panic!("{x:?}"),
    }
}

#[test]
fn test_lambdas_adv_2_rep_captures() {
    let script = format!(include_str!("script-template.xml"),
        globals = "", fields = "",
        funcs = "", methods = "",
        scripts = r##"<script x="75" y="89.02380952380952"><block s="doDeclareVariables"><list><l>a</l><l>b</l></list></block><block s="doSetVar"><l>a</l><block s="reifyReporter"><autolambda><block s="reportDifference"><block s="reportProduct"><block s="reportSum"><block var="a"/><block var="b"/></block><block var="b"/></block><block var="a"/></block></autolambda><list></list></block></block><block s="doSetVar"><l>b</l><block s="reifyScript"><script><block s="doDeclareVariables"><list><l>temp</l></list></block><block s="doSetVar"><l>temp</l><block var="ght"/></block><block s="doSetVar"><l>b</l><block s="reportPower"><block var="temp"/><block var="brg"/></block></block><block s="doSetVar"><l>b</l><block s="reportLessThan"><block var="temp"/><block var="b"/></block></block></script><list><l>ght</l><l>brg</l></list></block></block></script>"##,
    );
    let parser = Parser { omit_nonhat_scripts: false, ..Default::default() };
    let ast = parser.parse(&script).unwrap();
    let stmts = &ast.roles[0].entities[0].scripts[0].stmts;
    assert_eq!(stmts.len(), 3);
    match &stmts[1].kind {
        StmtKind::Assign { value, .. } => match &**value {
            Expr { kind: ExprKind::Closure { kind: _, params, captures, stmts }, .. } => {
                assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), Vec::<&str>::new());
                assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), vec!["a", "b"]);
                assert_eq!(stmts.len(), 1);
                match &stmts[0].kind {
                    StmtKind::Return { .. } => (),
                    x => panic!("{x:?}"),
                }
            }
            x => panic!("{x:?}"),
        }
        x => panic!("{x:?}"),
    }
    match &stmts[2].kind {
        StmtKind::Assign { value, .. } => match &**value {
            Expr { kind: ExprKind::Closure { kind: _, params, captures, stmts }, .. } => {
                assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), vec!["ght", "brg"]);
                assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), vec!["b"]);
                assert_eq!(stmts.len(), 4);
            }
            x => panic!("{x:?}"),
        }
        x => panic!("{x:?}"),
    }
}

#[test]
fn test_lambdas_adv_3_nested_captures() {
    let script = format!(include_str!("script-template.xml"),
        globals = "", fields = "",
        funcs = "", methods = "",
        scripts = r##"<script x="75" y="89.02380952380952"><block s="doDeclareVariables"><list><l>a</l><l>b</l></list></block><block s="doSetVar"><l>a</l><block s="reifyReporter"><autolambda><block s="reportDifference"><block s="reportProduct"><block s="reportSum"><block var="a"/><block var="b"/></block><block var="foo"/></block><block var="a"/></block></autolambda><list><l>foo</l></list></block></block><block s="doSetVar"><l>b</l><block s="reifyScript"><script><block s="doDeclareVariables"><list><l>temp</l></list></block><block s="doSetVar"><l>temp</l><block s="reifyReporter"><autolambda><block s="reportPower"><block var="b"/><block s="reportSum"><block var="temp"/><block var="brg"/></block></block></autolambda><list></list></block></block></script><list><l>ght</l><l>brg</l></list></block></block></script>"##,
    );
    let parser = Parser { omit_nonhat_scripts: false, ..Default::default() };
    let ast = parser.parse(&script).unwrap();
    let stmts = &ast.roles[0].entities[0].scripts[0].stmts;
    assert_eq!(stmts.len(), 3);
    match &stmts[1].kind {
        StmtKind::Assign { value, .. } => match &**value {
            Expr { kind: ExprKind::Closure { kind: _, params, captures, stmts }, .. } => {
                assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), vec!["foo"]);
                assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), vec!["a", "b"]);
                assert_eq!(stmts.len(), 1);
                match &stmts[0].kind {
                    StmtKind::Return { .. } => (),
                    x => panic!("{x:?}"),
                }
            }
            x => panic!("{x:?}"),
        }
        x => panic!("{x:?}"),
    }
    match &stmts[2].kind {
        StmtKind::Assign { value, .. } => match &**value {
            Expr { kind: ExprKind::Closure { kind: _, params, captures, stmts }, .. } => {
                assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), vec!["ght", "brg"]);
                assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), vec!["b"]);
                assert_eq!(stmts.len(), 2);
                match &stmts[1].kind {
                    StmtKind::Assign { value, .. } => match &**value {
                        Expr { kind: ExprKind::Closure { kind: _, params, captures, stmts }, .. } => {
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
                    x => panic!("{:?}", x),
                }
            }
            x => panic!("{:?}", x),
        }
        x => panic!("{:?}", x),
    }
}

#[test]
fn test_lambdas_script_capture() {
    let script = format!(include_str!("script-template.xml"),
        globals = "", fields = "",
        funcs = "", methods = "",
        scripts = r##"<script x="80.71428571428572" y="89.73809523809524"><block collabId="item_0" s="doDeclareVariables"><list><l>a</l></list></block><block collabId="item_1" s="doSetVar"><l>a</l><block collabId="item_4" s="reifyScript"><script><block collabId="item_3" s="doReplaceInList"><l>1</l><block collabId="item_5" var="a"/><l>thing</l></block></script><list></list></block></block></script>"##,
    );
    let parser = Parser { omit_nonhat_scripts: false, ..Default::default() };
    let ast = parser.parse(&script).unwrap();
    let stmts = &ast.roles[0].entities[0].scripts[0].stmts;
    assert_eq!(stmts.len(), 2);
    match &stmts[1].kind {
        StmtKind::Assign { value, .. } => match &**value {
            Expr { kind: ExprKind::Closure { kind: _, params, captures, stmts }, .. } => {
                assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), &[] as &[&str]);
                assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<&str>>(), vec!["a"]);
                assert_eq!(stmts.len(), 1);
                match &stmts[0].kind {
                    StmtKind::ListAssign { .. } => (),
                    x => panic!("{:?}", x),
                }
            }
            x => panic!("{:?}", x),
        }
        x => panic!("{:?}", x),
    }
}

#[test]
fn test_inline_rpc_metadata() {
    let script = format!(include_str!("script-template.xml"),
        globals = "", fields = "",
        funcs = "", methods = "",
        scripts = r##"<script x="15.384615384615383" y="15.384615384615383"><block collabId="item_11" s="doRunRPC" inputNames=""><l>TheCatApi</l><l>getCatBreeds</l></block><block collabId="item_14" s="doRunRPC" inputNames="sleepTime"><l>TimeSync</l><l>prepare</l><l></l></block><block collabId="item_17" s="doRunRPC" inputNames="startDate;stopDate;species;latitude;longitude;radius"><l>Wildcam</l><l>search</l><l></l><l></l><l></l><l></l><l></l><l></l></block></script>"##,
    );
    let parser = Parser { omit_nonhat_scripts: false, ..Default::default() };
    let ast = parser.parse(&script).unwrap();
    let stmts = &ast.roles[0].entities[0].scripts[0].stmts;
    assert_eq!(stmts.len(), 3);

    match &stmts[0].kind {
        StmtKind::RunRpc { service, rpc, args } => {
            assert_eq!(service, "TheCatApi");
            assert_eq!(rpc, "getCatBreeds");
            assert_eq!(args.iter().map(|x| x.0.as_str()).collect::<Vec<_>>(), &[] as &[&str]);
        }
        x => panic!("{:?}", x),
    }
    match &stmts[1].kind {
        StmtKind::RunRpc { service, rpc, args } => {
            assert_eq!(service, "TimeSync");
            assert_eq!(rpc, "prepare");
            assert_eq!(args.iter().map(|x| x.0.as_str()).collect::<Vec<_>>(), &["sleepTime"]);
        }
        x => panic!("{:?}", x),
    }
    match &stmts[2].kind {
        StmtKind::RunRpc { service, rpc, args } => {
            assert_eq!(service, "Wildcam");
            assert_eq!(rpc, "search");
            assert_eq!(args.iter().map(|x| x.0.as_str()).collect::<Vec<_>>(), &["startDate", "stopDate", "species", "latitude", "longitude", "radius"]);
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
    let parser = Parser { omit_nonhat_scripts: false, ..Default::default() };
    let ast = parser.parse(&script).unwrap();
    let stmts = &ast.roles[0].entities[0].scripts[0].stmts;
    assert_eq!(stmts.len(), 5);
    match &stmts[1].kind {
        StmtKind::Assign { value, .. } => match &**value {
            Expr { kind: ExprKind::CallClosure { closure, args, .. }, .. } => {
                match &closure.kind {
                    ExprKind::Closure { kind: _, params, captures, stmts } => {
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
        x => panic!("{:?}", x),
    }
    match &stmts[2].kind {
        StmtKind::Assign { value, .. } => match &**value {
            Expr { kind: ExprKind::CallClosure { closure, args, .. }, .. } => {
                match &closure.kind {
                    ExprKind::Closure { kind: _, params, captures, stmts } => {
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
        x => panic!("{:?}", x),
    }
    match &stmts[3].kind {
        StmtKind::RunClosure { closure, args, .. } => match &**closure {
            Expr { kind: ExprKind::Closure { kind: _, params, captures, stmts }, .. } => {
                assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<_>>(), Vec::<&str>::new());
                assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<_>>(), vec!["a"]);
                assert_eq!(stmts.len(), 1);
                assert_eq!(args.len(), 0);
            }
            x => panic!("{:?}", x),
        }
        x => panic!("{:?}", x),
    }
    match &stmts[4].kind {
        StmtKind::RunClosure { closure, args, .. } => match &**closure {
            Expr { kind: ExprKind::Closure { kind: _, params, captures, stmts }, .. } => {
                assert_eq!(params.iter().map(|x| x.name.as_str()).collect::<Vec<_>>(), vec!["val", "rgt"]);
                assert_eq!(captures.iter().map(|x| x.name.as_str()).collect::<Vec<_>>(), vec!["a", "b"]);
                assert_eq!(stmts.len(), 1);
                assert_eq!(args.len(), 2);
            }
            x => panic!("{:?}", x),
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
    let parser = Parser { omit_nonhat_scripts: false, ..Default::default() };
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
    let parser = Parser { omit_nonhat_scripts: false, ..Default::default() };

    parser.parse(include_str!("projects/raw-role-export.xml")).unwrap();
    parser.parse(include_str!("projects/role-export.xml")).unwrap();
    parser.parse(include_str!("projects/project-export.xml")).unwrap();
}

#[test]
fn test_empty_blocks() {
    let script = format!(include_str!("script-template.xml"),
        globals = "", fields = "",
        funcs = r#"<block-definition collabId="item_0" s="foo" type="command" category="custom"><header></header><code></code><translations></translations><inputs></inputs></block-definition><block-definition collabId="item_1" s="bar" type="command" category="custom"><header></header><code></code><translations></translations><inputs></inputs><script><block collabId="item_2" s="doDeclareVariables"><list><l>a</l></list></block></script></block-definition>"#,
        methods = "",
        scripts = "",
    );
    let parser = Parser { omit_nonhat_scripts: false, ..Default::default() };
    let ast = parser.parse(&script).unwrap();
    assert_eq!(ast.roles.len(), 1);
    match ast.roles[0].funcs.as_slice() {
        [foo, bar] => {
            assert_eq!(foo.name, "foo");
            assert_eq!(foo.stmts.len(), 0);
            assert_eq!(foo.upvars.len(), 0);

            assert_eq!(bar.name, "bar");
            assert_eq!(bar.stmts.len(), 1);
            assert_eq!(bar.upvars.len(), 0);
        }
        x => panic!("{x:?}"),
    }
}

#[test]
fn test_ref_inits() {
    let script = format!(include_str!("script-template.xml"),
        globals = r#"
<variable name="test1"><list struct="atomic" id="50">1,5,text,3</list></variable>
<variable name="test2"><ref id="50"></ref></variable>
<variable name="test3"><list collabId="" id="51"><item><l>1</l></item><item><l>5</l></item><item><l>text</l></item><item><ref id="50"></ref></item></list></variable>
<variable name="test4"></variable>
        "#,
        fields = "",
        funcs = "",
        methods = "",
        scripts = "",
    );
    let parser = Parser { omit_nonhat_scripts: false, ..Default::default() };
    let ast = parser.parse(&script).unwrap();
    assert_eq!(ast.roles.len(), 1);

    match ast.roles[0].globals.as_slice() {
        [test1, test2, test3, test4] => {
            assert_eq!(test1.def.name, "test1");
            assert_eq!(test2.def.name, "test2");
            assert_eq!(test3.def.name, "test3");
            assert_eq!(test4.def.name, "test4");

            match &test1.init {
                Value::List(values, ref_id) => {
                    assert_eq!(values.len(), 4);
                    match &values[0] {
                        Value::String(x) => assert_eq!(x, "1"),
                        x => panic!("{x:?}"),
                    }
                    match &values[1] {
                        Value::String(x) => assert_eq!(x, "5"),
                        x => panic!("{x:?}"),
                    }
                    match &values[2] {
                        Value::String(x) => assert_eq!(x, "text"),
                        x => panic!("{x:?}"),
                    }
                    match &values[3] {
                        Value::String(x) => assert_eq!(x, "3"),
                        x => panic!("{x:?}"),
                    }
                    assert_eq!(ref_id.as_ref().unwrap().0, 50);
                }
                x => panic!("{x:?}"),
            }
            match &test2.init {
                Value::Ref(ref_id) => assert_eq!(ref_id.0, 50),
                x => panic!("{x:?}"),
            }
            match &test3.init {
                Value::List(values, ref_id) => {
                    assert_eq!(values.len(), 4);
                    match &values[0] {
                        Value::String(x) => assert_eq!(x, "1"),
                        x => panic!("{x:?}"),
                    }
                    match &values[1] {
                        Value::String(x) => assert_eq!(x, "5"),
                        x => panic!("{x:?}"),
                    }
                    match &values[2] {
                        Value::String(x) => assert_eq!(x, "text"),
                        x => panic!("{x:?}"),
                    }
                    match &values[3] {
                        Value::Ref(ref_id) => assert_eq!(ref_id.0, 50),
                        x => panic!("{x:?}"),
                    }
                    assert_eq!(ref_id.as_ref().unwrap().0, 51);
                }
                x => panic!("{x:?}"),
            }
        }
        x => panic!("{x:?}"),
    }
}

#[test]
fn test_upvars() {
    let script = format!(include_str!("script-template.xml"),
        globals = "",
        fields = "",
        funcs = r#"<block-definition collabId="item_0_5" s="my for loop %&apos;i&apos; %&apos;action&apos;" type="command" category="custom"><header></header><code></code><translations></translations><inputs><input type="%upvar"></input><input type="%cs"></input></inputs></block-definition>"#,
        methods = "",
        scripts = "",
    );
    let parser = Parser { omit_nonhat_scripts: false, ..Default::default() };
    let ast = parser.parse(&script).unwrap();
    assert_eq!(ast.roles.len(), 1);
    let role = &ast.roles[0];
    assert_eq!(role.funcs.len(), 1);
    let func = &role.funcs[0];
    assert_eq!(func.upvars.len(), 1);
    assert_eq!(func.upvars[0].name, "i");
}

#[test]
#[allow(unreachable_code)]
fn test_stack_size_usage() {
    #[cfg(not(target_pointer_width = "64"))]
    panic!("not 64-bit system! so this test is meaningless!");
    #[cfg(debug_assertions)]
    panic!("not in release mode! so this test is meaningless!");

    macro_rules! assert_compiles {
        (src: $src:literal, stack_size: $stack_size:expr) => {
            std::thread::Builder::new().name($src.into()).stack_size($stack_size).spawn(|| {
                Parser::default().parse(include_str!($src)).unwrap();
            }).unwrap().join().unwrap();
        }
    }

    assert_compiles!(src: "projects/stack-size-1.xml", stack_size: 16 * 1024); // this is min stack size on linux
    assert_compiles!(src: "projects/stack-size-2.xml", stack_size: 16 * 1024); // this is min stack size on linux
    assert_compiles!(src: "projects/stack-size-3.xml", stack_size: 34 * 1024);
}
