use crate::commands::*;

use nom::branch::alt;
use nom::bytes::complete::{is_a, tag};
use nom::character::complete::{anychar, char, digit1, line_ending, one_of, space0, u8};
use nom::character::is_digit;
use nom::combinator::{eof, map, map_res, opt, verify, peek};
use nom::multi::many_till;
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::IResult;

fn eol_or_complete(input: &str) -> IResult<&str, ()> {
    map(alt((line_ending, peek(eof))), |_| ())(input)
}

fn comment(input: &str) -> IResult<&str, ()> {
    map(
        preceded(tag("//"), many_till(anychar, eol_or_complete)),
        |_| (),
    )(input)
}

fn whitespace(input: &str) -> IResult<&str, ()> {
    map(terminated(space0, opt(comment)), |_| ())(input)
}

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

fn empty_line(input: &str) -> IResult<&str, ()> {
    terminated(whitespace, eol_or_complete)(input)
}

fn command_line(input: &str) -> IResult<&str, Command> {
    terminated(
        delimited(
            whitespace,
            alt((a_command, l_command, c_command)),
            whitespace,
        ),
        eol_or_complete,
    )(input)
}

fn line(input: &str) -> IResult<&str, Option<Command>> {
    alt((map(command_line, Some), map(empty_line, |_| None)))(input)
}

fn lines(input: &str) -> IResult<&str, Vec<Command>> {
    terminated(
        map(many_till(line, eof), |(lines, _)| {
            lines.iter().flatten().map(Command::clone).collect()
        }),
        eof,
    )(input)
}

#[test]
fn testit() {
    assert_eq!(comment("// abcdefg"), Ok(("", ())));
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

    assert_eq!(line(""), Ok(("", None)));
    assert_eq!(line("     // The beginning "), Ok(("", None)));
    assert_eq!(line("// The beginning "), Ok(("", None)));
    assert_eq!(line("     // The beginning \r\n"), Ok(("", None)));
    assert_eq!(line("  \t  "), Ok(("", None)));
    assert_eq!(
        line(" D=D|M  // Label"),
        Ok((
            "",
            Some(Command::C(
                Some(Lhs::D),
                Expression::Binary(Lhs::D, BinaryOp::Or, Rhs::M),
                None
            ))
        ))
    );
    assert_eq!(
        line(" (LABEL)  // Label"),
        Ok(("", Some(Command::L("LABEL".to_string()))))
    );

    assert_eq!(eol_or_complete("\r\n"), Ok(("", ())));
    assert_eq!(eol_or_complete(""), Ok(("", ())));
    assert_eq!(empty_line("// Of the end\r\n"), Ok(("", ())));
    assert_eq!(lines("\n\n"), Ok(("", vec![])));
    assert_eq!(lines("     // The beginning \n// Of the end\r\n"), Ok(("", vec![])));

    assert_eq!(
        lines("     // The beginning \n// Of the end\r\n(LABEL) \n   @12\n @B_NUT12$\nD=M+1\n"),
        Ok((
            "",
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
        ))
    )
}
