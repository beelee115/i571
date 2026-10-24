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
    let env = Env::<EvalValue>::new();
    eval_in_env(ast, &env)
}

fn eval_in_env(ast: &ExprNode, env: &Env<EvalValue>) -> EvalResult {
    match ast {
	// dereference and wrap as EvalValue
	ExprNode::Int { value, .. } => Ok(EvalValue::Value(*value)),
	// lambda_expr 
	// op_fn takes binary operator and returns a rust closure that performs actual arithmetic calc
	ExprNode::Op2 { op, rand1, rand2, coord } => {
	    let f = op_fn(op, coord)?;
		// recursively evalute the two operands here - reference to exprnode, reference to evalnode
		// need to go from an EvalValue to a raw i64 - use val helper
		let left = eval_in_env(rand1, env)?.val(coord)?;
		let right = eval_in_env(rand2, env)?.val(coord)?;
		// look for division by zero
		if (op == "/" || op == "%") && right == 0 {
			return Err(EvalError(format!("{}: division by zero", coord)));
		}
		Ok(EvalValue::Value(f(left, right)))
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
	ExprNode:: Let { let_kind, defs, expr, .. } => {
		// use the helper functions
		match let_kind {
			LetKind::Parallel => eval_let(defs, expr, env),
			LetKind::Sequential => eval_let_star(defs, expr, env),
			LetKind::Recursive => eval_letrec(defs, expr, env),
		}
	},
	// Need to look up env using env.get()
	ExprNode::Id { id, coord } => {
		// ok or else transofmrs option into result
		// we need to return an 'Ok Result' so use that
		// https://doc.rust-lang.org/std/option/enum.Option.html#method.ok_or_else
		let v = env.get(id).ok_or_else(|| {
			EvalError(format!("{}: unbound identifier {}", coord, id))
		})?;
		// need lookup fails --> semantic error
		if let EvalValue::Undef = v {
			return Err(EvalError(format!("{}: {} was used before it was defined", coord, id)));
		}
		// otherwise return Ok Result
		Ok(v)
	},
	// use auxiliary function
	ExprNode::App { name, args, coord } => eval_app(name, args, coord, env), 

	// we panic below because not handling an AST variant is a bug
	_ => panic!("eval not defined for ast variant {:?}", ast),
    }
}

/*************************** Helpers for Eval **************************/

// Eval let function helpers defined here
// for each id = expr definition in defs, evaluate expr in the original environment to some value
// create new environment with the binding id=value pushed onto it
fn eval_let(defs: &[ExprNode], expr: &ExprNode, env: &Env<EvalValue>) -> EvalResult {
	let mut env1 = env.clone();
	// d must be a reference to the def variant of exprnode
	for d in defs {
		// based on prj 2 instructions
		if let ExprNode::Def {id, expr: def, ..} = d {
			let v = eval_in_env(def, env)?;
			env1 = env1.push(id, v);
		}
	}
	eval_in_env(expr, &env1)
}

fn eval_let_star(defs: &[ExprNode], expr: &ExprNode, env: &Env<EvalValue>) -> EvalResult{
	let mut env1 = env.clone();
	for d in defs {
		// based on prj 2 instructions
		if let ExprNode::Def {id, expr: def, ..} = d {
			let v = eval_in_env(def, &env1)?;
			env1 = env1.push(id, v);
		}
	}
	eval_in_env(expr, &env1)
}

fn eval_letrec(defs: &[ExprNode], expr: &ExprNode, env: &Env<EvalValue>) -> EvalResult{
	let mut env1 = env.clone();
	for d in defs {
		// based on prj 2 instructions
		if let ExprNode::Def {id, ..} = d {
			env1 = env1.push(id, EvalValue::Undef);
		}
	}
	// make identifiers visible using undef placeholders
	for d in defs {
		if let ExprNode::Def {id, expr: def, ..} = d{
			let v = eval_in_env(def, &env1)?;
			env1.set(id, v);
		}
	}
	eval_in_env(expr, &env1)
}

// helper for App -- semantic errors below
// An unknown binary operator is used. 
// Expecting an integer value but got something else. 
// Use of an undefined identifier.
// Use of an unknown function name. 
// Use of an identifier as a function name which is not bound to a lambda. 
// The number of actual parameters provided in a function application do not match the number of formal parameters.
fn eval_app(name: &str, args: &[ExprNode], coord: &Coord, env: &Env<EvalValue>) -> EvalResult {
	// look up the function name -> error if function name unknown
	let f = env.get(name).ok_or_else(|| {EvalError(format!("{}: unknown function {}", coord, name))
	})?;
	// closure represents a lambda so must be closure 
	if let EvalValue::Closure{params, body, env: closure_env} = f{
		// wrong number of actual parameters
		if params.len() != args.len() {
			return Err(EvalError(format!(
				"{}: {} expects {} arguments and we got {}",
				coord, name, params.len(), args.len()
			)));
		}

		// bind the formal to its actual
		let mut call_env = closure_env; //start captured env
		for i in 0..params.len() {
			// evaluate each actual argument in the caller's env
			let v = eval_in_env(&args[i], env)?;
			// add parameter binding to func environment
			call_env = call_env.push(&params[i], v);
		}

		// evaluate body in the extended env
		eval_in_env(&body, &call_env)
	} else {
		Err(EvalError(format!("{}: {} is not a function", coord, name)))
	}
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
    Err(EvalError(format!("{}: unknown binary operation {}", coord, op)))
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


