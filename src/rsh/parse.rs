#![allow(dead_code)]
pub mod ast {
    use super::redir::Redir;
    use super::case::CaseInfo;
    use super::semantics::Range;
    use super::redir::RedirType;

    #[derive(Debug, PartialEq)]
    pub enum ASTNode {
        Null,
        Command {
            name: String,
            args: Vec<String>,
        },
        Chain {
            left: Box<ASTNode>,
            right: Box<ASTNode>,
            operator: ChainLogic,
        },
        Conditional {
            main_block: Box<CondPath>,
            elif_blocks: Vec<CondPath>,
            else_block: Option<Box<CondPath>>,
        },
        WhileLoop {
            condition: Box<ASTNode>,
            body: Vec<ASTNode>,
        },
        UntilLoop {
            condition: Box<ASTNode>,
            body: Vec<ASTNode>,
        },
        ForLoop {
            loop_var: String,
            loop_array: String,
            body: Vec<ASTNode>,
        },
        Case {
            check_var: Box<Unit>,
            elements: Vec<CaseInfo>,
        },
        Pipeline {
            left: Box<ASTNode>,
            right: Box<ASTNode>,
        },
        FuncDef {
            func_name: String,
            func_body: Vec<ASTNode>,
        },
        Subshell {
            interpreter: Option<String>,
            body: Vec<ASTNode>,
        },
    }

    #[derive(Debug, PartialEq)]
    pub enum CondPath {
        Null,
        If {
            condition: ASTNode,
            body: Vec<ASTNode>,
        },
        Elif {
            condition: ASTNode,
            body: Vec<ASTNode>,
        },
        Else {
            body: Vec<ASTNode>,
        },
    }

    #[derive(Debug, PartialEq)]
    pub enum ChainLogic {
        And,
        Or,
    }

    #[derive(Debug, PartialEq)]
    pub enum LoopType {
        For,
        While,
        Until,
    }

    #[derive(Debug, PartialEq)]
    pub enum Unit {
        Path {
            kind: PathType,
            target: String,
        },
        Variable {
            id: String,
        },
        ArrayVar {
            elements: Vec<String>,
        },
        FileDescriptor {
            id: i32,
        },
        ProcessSub {
            exec: Box<ASTNode>,
        },
    }

    #[derive(Debug, PartialEq)]
    pub enum PathType {
        Absolute,
        Relative,
    }
}

pub mod redir {
    use super::ast::Unit;

    #[derive(Debug, PartialEq)]
    pub enum Redir {
        Simple {
            dir: RedirType,
            target: Unit,
        },
        FileDesc {
            dir: RedirType,
            fd_out: Option<Unit>,
            target: Option<Unit>,
        },
        Heredoc {
            body: String,
            strip_tabs: bool, // Replaces HeredocType enum
        },
    }

    #[derive(Debug, PartialEq)]
    pub enum RedirType {
        Input,      // `<`
        Output,     // `>`
        Append,     // `>>`
        Herestring, // `<<<`
    }
}

pub mod case {
    use super::ast::{ASTNode, Unit};

    #[derive(Debug, PartialEq)]
    pub struct CaseInfo {
        pub check_var: Unit,
        pub body: Vec<ASTNode>,
    }
}

pub mod semantics {
    #[derive(Debug, PartialEq)]
    pub enum Range {
        Numeric { start: i32, end: i32 },
        Alphabetic { start: char, end: char },
    }

    #[derive(Debug, PartialEq)]
    pub struct Test {
        pub condition: String, // You might want this to be an ASTNode for complex conditions
    }
}
