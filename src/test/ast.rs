use crate::*;

#[test]
fn test_opt_cmp() {
    let script = format!(include_str!("script-template.xml"), scripts = r#"
    <script x="0" y="0"><block s="doDeclareVariables"><list><l>temp</l></list></block><block s="doSetVar"><l>temp</l><block s="reportEquals"><block var="temp"/><block var="temp"/></block></block><block s="doSetVar"><l>temp</l><block s="reportNot"><block s="reportEquals"><block var="temp"/><block var="temp"/></block></block></block><block s="doSetVar"><l>temp</l><block s="reportLessThan"><block var="temp"/><block var="temp"/></block></block><block s="doSetVar"><l>temp</l><block s="reportNot"><block s="reportLessThan"><block var="temp"/><block var="temp"/></block></block></block><block s="doSetVar"><l>temp</l><block s="reportGreaterThan"><block var="temp"/><block var="temp"/></block></block><block s="doSetVar"><l>temp</l><block s="reportNot"><block s="reportGreaterThan"><block var="temp"/><block var="temp"/></block></block></block></script>
    "#);
    let parser = ParserBuilder::default().omit_nonhat_scripts(false).build().unwrap();
    let ast = parser.parse(&mut script.as_bytes()).unwrap();
    let stmts = &ast.roles[0].sprites[0].scripts[0].stmts;
    assert_eq!(stmts.len(), 7);
    match &stmts[1] {
        Stmt::Assign { value, .. } => match value {
            Expr::Eq { .. } => (),
            x => panic!("{:#?}", x),
        }
        x => panic!("{:#?}", x),
    }
}
