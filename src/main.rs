use std::fmt::Write;
use std::fs::File;

use graphviz_rust::cmd::CommandArg;
use graphviz_rust::cmd::Format;
use graphviz_rust::dot_generator::*;
use graphviz_rust::dot_structures::*;
use graphviz_rust::printer::DotPrinter;
use graphviz_rust::printer::PrinterContext;
use relua::dominator::DominatorTree;
use relua::{cfg::ControlFlowGraph, parse};

use relua::stmt::BlockStatements;
use relua::stmt::Variables;

fn main() {
    let mut file = File::open("lua-5.1.5/test.luac").unwrap();
    // [27, 76, 117, 97, 81]
    let func = parse(&mut file);

    println!("{:#?}", func);
    println!("{}", func.code);
    let cfg = ControlFlowGraph::build(&func.code).unwrap();
    println!("{:#?}", cfg);

    let dominator_tree = DominatorTree::from_cfg(&cfg);

    let mut g = graph!(strict di id!('a'));
    for node in cfg.graph.iter() {
        let id = format!("{}", node.id());

        // Generate label from instructions
        let mut buf = format!(
            "Block {} (idom: {})\\l",
            node.id(),
            dominator_tree.immediate_dominator(node.id()).unwrap()
        );
        for (idx, instruction) in node
            .get()
            .iter(&func.code)
            .map(|(idx, i)| (idx, i.unwrap()))
        {
            write!(buf, "{}  {}\\l", idx, instruction).unwrap();
        }

        let label = format!("\"{}\"", buf.replace('\n', "\\n"));
        g.add_stmt(node!(id; attr!("label", label), attr!("shape", "square")).into());
        for link in node.links_to() {
            let to_id = node_id!(format!("{}", link));
            g.add_stmt(edge!(node_id!(id) => to_id).into());
        }
    }

    let mut variables = Variables::new();

    /*
    for block in cfg.iter() {
        println!("{:?}", block.id);
        let sb = BlockStatements::build(block, &func.code, &mut variables).unwrap();
        println!("{:#?}", sb.statements);
    }*/

    //println!("{}", g.print(&mut PrinterContext::default()));

    graphviz_rust::exec(
        g,
        &mut PrinterContext::default(),
        vec![
            Format::Svg.into(),
            CommandArg::Output("graph.svg".to_string()),
        ],
    )
    .unwrap();

    relua::decompile::decompile(&cfg, &func.code).unwrap();
}
