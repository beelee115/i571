use crate::lexer::{Coord};
use crate::parser::{ExprNode, Value, LetKind};
use crate::env::{Env};

use std::fmt;
use std::error::Error;


/************************* Expression Evaluator ************************/

/// eval() Result possibilities: an EvalValue success or an
/// EvalError failure
type EvalResult = Result<EvalValue, EvalError>;

pub fn eval(ast: &ExprNode) -> EvalResult {
    //simply delegate to eval_in_env() with a new env.
    let env = &Env::<EvalValue>::new();
    eval_in_env(ast, &env)
}

fn eval_in_env(ast: &ExprNode, env: &Env<EvalValue>) -> EvalResult {
    match ast {
	// dereference and wrap as EvalValue
	ExprNode::Int { value, .. } => Ok(EvalValue::Value(*value)),
	// lambda_expr 
	// op_fn takes binary operator and returns a rust closure that performs actual arithmetic calc
	ExprNode::Op2 { op, rand1, rand2, coord } => {
	    let op_fn = op_fn(op, coord)?;
		// recursively evalute the two operands here - reference to exprnode, reference to evalnode
		// need to go from an EvalValue to a raw i64 - use val helper
		let left = eval_in_env(rand1, env)?.val(coord)?;
		let right = eval_in_env(rand2, env)?.val(coord)?;
		// look for division by zero
		if (op == "/" || op == "%") && right == 0 {
			return Err(EvalError(format!("{}: division by zero", coord)));
		}
		Ok(EvalValue::Value(op_fn(left, right)))
	},
	ExprNode::If { cond, then, else_, coord } => {
		// should be similar to op2 with using .val(coord)?
	    if eval_in_env(cond, env)?.val(coord)? != 0 {
			eval_in_env(then, env)
		}
		else {
			eval_in_env(else_, env)
		}
	},
	// clone the params because closures own their own fields, eval_in_env borrows
	ExprNode::Lambda { params, body, .. } => Ok(EvalValue::Closure {
		//.clone as to not take ownership
		params: params.clone(),
		body: (**body).clone(),
		env: env.clone(),
	}),
	ExprNode:: Let { let_kind, defs, expr, .. } => match let_kind {
	    LetKind::Parallel => eval_let(),
		LetKind::Sequential => eval_let_star(),
		LetKind::Recursive => eval_letrec(),
	}
	ExprNode::Id { id, coord } => todo!(),

	ExprNode::App { name, args, coord } => todo!(),

	// we panic below because not handling an AST variant is a bug
	_ => panic!("eval not defined for ast variant {:?}", ast),
    }
}

/*************************** Helpers for Eval **************************/

// Eval let function helpers defined here
// for each id = expr definition in defs, evaluate expr in the original environment to some value
// create new environment with the binding id=value pushed onto it
fn eval_let() {

}

fn eval_let_star() {

}

fn eval_letrec() {

}
/**************************** Op2 Helper *******************************/

/// Avoid boilerplace for the different Op2 functions.  Simply call
/// op_fn() on the op and then apply the returned BinOpFn to
/// evaluate the recursively evaluted operands.

type BinOpFn = fn(Value, Value) -> Value;
struct OpFunc(&'static str,BinOpFn);

fn op_fn(op: &str, coord: &Coord) -> Result<BinOpFn, EvalError> {
    for op_fn in OP_FUNCS {
	if op == op_fn.0 {
	    return Ok(op_fn.1);
	}
    }
    Err(EvalError(format!("{}: unknown binary operation {}", op, coord)))
}


const OP_FUNCS: [OpFunc;7] = [
    OpFunc("+", |a, b| a+b),
    OpFunc("-", |a, b| a-b),
    OpFunc("*", |a, b| a*b),
    OpFunc("/", |a, b| a/b),
    OpFunc("%", |a, b| a%b),
    OpFunc("==", |a, b| if a == b { 1 } else { 0 }),
    OpFunc("<", |a, b| if a < b { 1 } else { 0 }),
];

/********************** Eval Results and Errors ************************/

/// The result of evaluating an expression
#[derive(Debug, Clone)]
pub enum EvalValue {
    Value(Value), //the expected value
    Closure { //captures a `lambda` and its referencing environment
	//owns its fields
	params: Vec<String>,
	body: ExprNode,
	env: Env<EvalValue>,
    },
    Undef,  //undefined value; use when implemented letrec
}

/// When an EvalValue is expected to be a Value, use .val() to
/// extract it without needing to pattern match. 
impl EvalValue {
    fn val(&self, coord: &Coord) -> Result<Value, EvalError> {
	if let Self::Value(value) = self {
	    Ok(*value)
	}
	else {
	    Err(EvalError(format!("{}: expected a Value, but got {:?}",
				  coord, self)))
	}
    }
}

/// Displaying EvalValue
impl fmt::Display for EvalValue {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
	match self {
	    Self::Value(val) => write!(f, "{}", val),
	    Self::Closure{..} => write!(f, "<closure>"),
	    Self::Undef => write!(f, "<undef>"),
	}
    }
}

/// Used to wrap an evaluation error message
#[derive(Debug, Clone)]
pub struct EvalError(String);

/// Displaying EvalError
impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
	write!(f, "{}", self.0)
    }
}

impl Error for EvalError {}


