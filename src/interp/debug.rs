use std::fmt;

use crate::interp;
use interp::parse::Node;

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn write_tree(node: &Node, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
            let prefix = "|   ".repeat(indent);
            match node {
                Node::Root { deck } => {
                    writeln!(f, "{}Root", prefix)?;
                    for node in deck {
                        write_tree(node, f, indent + 1)?;
                    }
                }
                Node::If { cond_blocks, else_block } => {
                    writeln!(f, "{}If", prefix)?;
                    for block in cond_blocks {
                        writeln!(f, "{}|--- Condition", prefix)?;
                        write_tree(&block.condition, f, indent + 2)?;
                        writeln!(f, "{}|--- Body", prefix)?;
                        write_tree(&block.body, f, indent + 2)?;
                    }
                    if let Some(else_block) = else_block {
                        writeln!(f, "{}|--- Else", prefix)?;
                        write_tree(else_block, f, indent + 1)?;
                    }
                }
                Node::For { loop_vars, loop_arr, loop_body } => {
                    let mut var_vec = vec![];
                    let mut arr_vec = vec![];
                    for var in loop_vars { var_vec.push(var.text()) }
                    for element in loop_arr { arr_vec.push(element.text()) }
                    writeln!(f, "{}For", prefix)?;
                    writeln!(f, "{}|--- Loop Vars: {:?}", prefix, var_vec)?;
                    writeln!(f, "{}|--- Loop Array: {:?}", prefix, arr_vec)?;
                    writeln!(f, "{}|--- Body", prefix)?;
                    write_tree(loop_body, f, indent + 1)?;
                }
                Node::Loop { condition, logic } => {
                    writeln!(f, "{}Loop (break condition: {})", prefix, condition)?;
                    writeln!(f,"{}|---Loop Condition:",prefix)?;
                    write_tree(&logic.condition, f, indent + 1)?;
                    writeln!(f,"{}|---Loop Body:",prefix)?;
                    write_tree(&logic.body, f, indent + 1)?;
                }
                Node::Case { input_var, cases } => {
                    writeln!(f, "{}Case (input: {:?})", prefix, input_var)?;
                    for case in cases {
                        writeln!(f, "{}|--- Pattern: {:?}", prefix, case.pattern)?;
                        write_tree(&case.logic, f, indent + 1)?;
                    }
                }
                Node::Select { select_var, opts, body } => {
                    writeln!(f, "{}Select (input: {:?}, options: {:?})", prefix, select_var, opts)?;
                    write_tree(body, f, indent + 1)?;
                }
                Node::Pipeline { left, right } => {
                    writeln!(f, "{}Pipeline", prefix)?;
                    writeln!(f, "{}|--- Left", prefix)?;
                    write_tree(left, f, indent + 1)?;
                    writeln!(f, "{}|--- Right", prefix)?;
                    write_tree(right, f, indent + 1)?;
                }
                Node::Chain { left, right, op } => {
                    writeln!(f, "{}Chain (operator: {:?})", prefix, op)?;
                    writeln!(f, "{}|--- Left", prefix)?;
                    write_tree(left, f, indent + 1)?;
                    writeln!(f, "{}|--- Right", prefix)?;
                    write_tree(right, f, indent + 1)?;
                }
                Node::BraceGroup { body } => {
                    writeln!(f, "{}BraceGroup", prefix)?;
                    write_tree(body, f, indent + 1)?;
                }
                Node::Subshell { body } => {
                    writeln!(f, "{}Subshell (body: {:?})", prefix, body)?;
                }
                Node::FuncDef { name, body } => {
                    writeln!(f, "{}Function Definition (name: {})", prefix, name)?;
                    write_tree(body, f, indent + 1)?;
                }
                Node::Assignment { name, value } => {
                    writeln!(f, "{}Assignment (name: {}, value: {:?})", prefix, name, value)?;
                }
                Node::Command { argv, redirs } => {
                    let mut args = vec![];
                    for arg in argv {
                        args.push(arg.text());
                    }
                    writeln!(f, "{}Command (args: {:?}, redirs: {:?})", prefix, args, redirs)?;
                }
                Node::And => {
                    writeln!(f, "{}And", prefix)?;
                }
                Node::Or => {
                    writeln!(f, "{}Or", prefix)?;
                }
                Node::Pipe => {
                    writeln!(f, "{}Pipe", prefix)?;
                }
                Node::Cmdsep => {
                    writeln!(f, "{}Cmdsep", prefix)?;
                }
            }
            Ok(())
        }

        write_tree(self, f, 0)
    }
}
