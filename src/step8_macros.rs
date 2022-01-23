extern crate mal;
use mal::ast::MalList;
use mal::ast::MalSymbol;
use mal::ast::MalType;
use mal::eval;
use mal::make_env;
use mal::print;
use mal::read_str;
use mal::Editor;
use mal::MalEnv;
use mal::ReadlineError;
use std::rc::Rc;

fn rep(line: &str, env: &Rc<MalEnv>) -> Result<String, Box<dyn std::error::Error>> {
    let ast = read_str(line)?;
    match eval(ast, env.clone()) {
        Ok(res) => Ok(print(&res, true)),
        Err(e) => Err(e),
    }
}

const NOT_DEFINITION: &str = "(def! not (fn* (a) (if a false true)))";
const LOAD_FILE: &str =
    r#"(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))"#;
const COND_DEFINITION: &str = "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))";

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut args = std::env::args();
    let mut rl = Editor::<()>::new();
    let env = make_env();
    let file = args.nth(1);
    rep(NOT_DEFINITION, &env)?;
    rep(LOAD_FILE, &env)?;
    rep(COND_DEFINITION, &env)?;
    env.insert(
        MalSymbol::new("*ARGV*"),
        MalType::List(MalList::new(
            args.into_iter().map(|a| MalType::Str(a.into())).collect(),
        )),
    );
    if let Some(file) = file {
        rep(&format!(r#"(load-file "{}")"#, file), &env)?;
        std::process::exit(0);
    }
    loop {
        let r = rl.readline("user> ");
        match r {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                if !line.is_empty() {
                    match rep(&line, &env) {
                        Ok(s) => println!("{}", s),
                        Err(e) => {
                            println!("Error: {}", e)
                        }
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                continue;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(e) => {
                println!("{}", e);
                break;
            }
        }
    }
    Ok(())
}
