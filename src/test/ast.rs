use crate::*;
use std::prelude::v1::*;

#[test]
fn test_opt_cmp() {
    let script = format!(include_str!("script-template.xml"),
        globals = "", fields = "",
        funcs = "", methods = "",
        scripts = r#"<script x="0" y="0"><block s="doDeclareVariables"><list><l>temp</l></list></block><block s="doSetVar"><l>temp</l><block s="reportEquals"><block var="temp"/><block var="temp"/></block></block><block s="doSetVar"><l>temp</l><block s="reportNot"><block s="reportEquals"><block var="temp"/><block var="temp"/></block></block></block><block s="doSetVar"><l>temp</l><block s="reportLessThan"><block var="temp"/><block var="temp"/></block></block><block s="doSetVar"><l>temp</l><block s="reportNot"><block s="reportLessThan"><block var="temp"/><block var="temp"/></block></block></block><block s="doSetVar"><l>temp</l><block s="reportGreaterThan"><block var="temp"/><block var="temp"/></block></block><block s="doSetVar"><l>temp</l><block s="reportNot"><block s="reportGreaterThan"><block var="temp"/><block var="temp"/></block></block></block></script>"#,
    );
    let parser = ParserBuilder::default().omit_nonhat_scripts(false).build().unwrap();
    let ast = parser.parse(&script).unwrap();
    let stmts = &ast.roles[0].entities[0].scripts[0].stmts;
    assert_eq!(stmts.len(), 7);
    match &stmts[1] {
        Stmt::Assign { value, .. } => match value {
            Expr::Eq { .. } => (),
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
    let parser = ParserBuilder::default().omit_nonhat_scripts(false).build().unwrap();
    parser.parse(&script).unwrap();
}
