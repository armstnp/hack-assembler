use crate::commands::*;

use nom::branch::alt;
use nom::bytes::complete::{is_a, tag};
use nom::character::complete::{char, digit1, one_of};
use nom::character::is_digit;
use nom::combinator::{map, map_res, opt, verify};
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::IResult;

fn char_to_lhs(c: char) -> Lhs {
    match c {
        'A' => Lhs::A,
        'D' => Lhs::D,
        'M' => Lhs::M,
        c => panic!("Parse for lhs provided unexpected char {}", c),
    }
}

fn lhs(input: &str) -> IResult<&str, Lhs> {
    map(one_of("01ADM"), char_to_lhs)(input)
}

fn rhs(input: &str) -> IResult<&str, Rhs> {
    map(one_of("01ADM"), |c| match c {
        '0' => Rhs::Zero,
        '1' => Rhs::One,
        'A' => Rhs::A,
        'D' => Rhs::D,
        'M' => Rhs::M,
        c => panic!("Parse for rhs provided unexpected char {}", c),
    })(input)
}

fn unop(input: &str) -> IResult<&str, UnaryOp> {
    map(opt(one_of("-!")), |c| match c {
        Some('-') => UnaryOp::Negate,
        Some('!') => UnaryOp::Not,
        None => UnaryOp::Identity,
        Some(c) => panic!("Parse for unop provided unexpected char {}", c),
    })(input)
}

fn unexpr(input: &str) -> IResult<&str, Expression> {
    map(tuple((unop, rhs)), |(op, rhs)| Expression::Unary(op, rhs))(input)
}

fn binop(input: &str) -> IResult<&str, BinaryOp> {
    map(one_of("+-&|"), |c| match c {
        '+' => BinaryOp::Add,
        '-' => BinaryOp::Subtract,
        '&' => BinaryOp::And,
        '|' => BinaryOp::Or,
        c => panic!("Parse for binop provided unexpected char {}", c),
    })(input)
}

fn binexpr(input: &str) -> IResult<&str, Expression> {
    let base = verify(tuple((lhs, binop, rhs)), |(l, _, r)| match (l, r) {
        (Lhs::A, Rhs::M) => false,
        (Lhs::M, Rhs::A) => false,
        (Lhs::A, Rhs::A) => false,
        (Lhs::D, Rhs::D) => false,
        (Lhs::M, Rhs::M) => false,
        _ => true,
    });
    map(base, |(lhs, op, rhs)| Expression::Binary(lhs, op, rhs))(input)
}

fn expr(input: &str) -> IResult<&str, Expression> {
    alt((binexpr, unexpr))(input)
}

fn dest(input: &str) -> IResult<&str, Lhs> {
    map(terminated(one_of("ADM"), char('=')), char_to_lhs)(input)
}

fn jump(input: &str) -> IResult<&str, JumpCondition> {
    map(
        preceded(
            tag(";J"),
            alt((
                tag("GT"),
                tag("EQ"),
                tag("GE"),
                tag("LT"),
                tag("NE"),
                tag("LE"),
                tag("MP"),
            )),
        ),
        |couplet| match couplet {
            "GT" => JumpCondition::Greater,
            "EQ" => JumpCondition::Equal,
            "GE" => JumpCondition::GreaterEqual,
            "LT" => JumpCondition::Less,
            "NE" => JumpCondition::NotEqual,
            "LE" => JumpCondition::LessEqual,
            "MP" => JumpCondition::Unconditional,
            c => panic!("Parse for jump provided unexpected jump code {}", c),
        },
    )(input)
}

fn c_command(input: &str) -> IResult<&str, Command> {
    map(tuple((opt(dest), expr, opt(jump))), |(dest, expr, jump)| {
        Command::C(dest, expr, jump)
    })(input)
}

fn addr_constant(input: &str) -> IResult<&str, Address> {
    map_res(digit1, |c: &str| c.parse().map(Address::Constant))(input)
}

fn symbol(input: &str) -> IResult<&str, String> {
    map(
        verify(
            is_a("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_.$:0123456789"),
            |c: &str| !is_digit(c.as_bytes()[0]),
        ),
        |sym: &str| sym.to_string(),
    )(input)
}

fn addr_symbol(input: &str) -> IResult<&str, Address> {
    map(symbol, Address::Symbol)(input)
}

fn a_command(input: &str) -> IResult<&str, Command> {
    map(
        preceded(tag("@"), alt((addr_symbol, addr_constant))),
        Command::A,
    )(input)
}

fn l_command(input: &str) -> IResult<&str, Command> {
    map(delimited(tag("("), symbol, tag(")")), Command::L)(input)
}

pub fn parse(input: &str) -> Vec<Command> {
    let mut commands = vec![];

    for line in input.lines() {
        let line = line.split_once("//").map(|(s, _)| s).unwrap_or(line).trim();
        if line.is_empty() {
            continue;
        }

        //let res = try_parse_a(line).or_else(try_parse_l).or_else(try_parse_c);

        let res = alt((a_command, l_command, c_command))(line);

        match res {
            Ok(("", command)) => commands.push(command),
            Ok((remainder, _)) => panic!("Command {} has extra parts {}", line, remainder),
            Err(line) => panic!("Invalid command {}", line),
        }
    }

    commands
}

#[test]
fn test_parse() {
    assert_eq!(
        parse("     // The beginning \n// Of the end\r\n(LABEL) \n   @12\n @B_NUT12$\nD=M+1\n"),
        vec![
            Command::L("LABEL".to_string()),
            Command::A(Address::Constant(12)),
            Command::A(Address::Symbol("B_NUT12$".to_string())),
            Command::C(
                Some(Lhs::D),
                Expression::Binary(Lhs::M, BinaryOp::Add, Rhs::One),
                None
            )
        ]
    )
}

#[test]
fn nom_parse_tests() {
    assert_eq!(
        expr("D+M"),
        Ok(("", Expression::Binary(Lhs::D, BinaryOp::Add, Rhs::M)))
    );
    assert_eq!(
        c_command("D=D|M;JEQ"),
        Ok((
            "",
            Command::C(
                Some(Lhs::D),
                Expression::Binary(Lhs::D, BinaryOp::Or, Rhs::M),
                Some(JumpCondition::Equal)
            )
        ))
    );
    assert_eq!(
        l_command("(LABEL)"),
        Ok(("", Command::L("LABEL".to_string())))
    );
    assert_eq!(
        a_command("@LABEL"),
        Ok(("", Command::A(Address::Symbol("LABEL".to_string()))))
    );
    assert_eq!(
        a_command("@12"),
        Ok(("", Command::A(Address::Constant(12))))
    );

    assert_eq!(symbol("LABEL"), Ok(("", "LABEL".to_string())));
}
