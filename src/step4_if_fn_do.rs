extern crate mal;
use mal::eval;
use mal::make_env;
use mal::print;
use mal::read_str;
use mal::Editor;
use mal::MalEnv;
use mal::ReadlineError;
use std::rc::Rc;

fn rep(line: &str, env: &Rc<MalEnv>) -> Result<(), Box<dyn std::error::Error>> {
    let ast = read_str(line)?;
    match eval(ast, env.clone()) {
        Ok(res) => println!("{}", print(&res, true)),
        Err(e) => {
            println!("Error in eval {}", e)
        }
    };
    Ok(())
}

const NOT_DEFINITION: &str = "(def! not (fn* (a) (if a false true)))";
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut rl = Editor::<()>::new();
    let env = make_env();
    rep(NOT_DEFINITION, &env)?;
    loop {
        let r = rl.readline("user> ");
        match r {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                if !line.is_empty() {
                    rep(&line, &env)?;
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
