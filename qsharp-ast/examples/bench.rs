use std::{env::args, fs, time::Instant};

use qsharp_ast::ast::Program;
use strip_bom::StripBom;
use syn::Result;

fn main() -> Result<()> {
    let mut total = 0;
    let mut fails = vec![];
    let mut count = 0;

    let mut args = args();

    args.next();

    for path in args {
        count += 1;

        let mut input = fs::read_to_string(&path).unwrap();

        input = input.trim().strip_bom().into();

        let now = Instant::now();

        match syn::parse_str::<Program>(&input) {
            Ok(_) => {
                let elapsed = now.elapsed().as_millis();
                println!("{} ms   {}", elapsed, path);
                total += elapsed
            }
            Err(err) => {
                println!("[e] {}   {}", err, path);
                fails.push(path);
            }
        }
    }

    println!("total: {} ms", total);
    println!("#fails: {} / {}", fails.len(), count);

    for p in fails {
        println!("[e] {}", p);
    }

    Ok(())
}
