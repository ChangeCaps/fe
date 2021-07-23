use crate::spanned::Span;

pub fn error_message(source: &str, span: Span, message: &str) -> String {
    let mut lo = 0;
    let lines = source
        .split("\n")
        .filter_map(|s| {
            let hi = lo + s.len();

            let res = if (span.lo >= lo && span.hi <= hi)
                || (span.lo <= lo && span.hi >= hi)
                || (span.lo >= lo && span.lo <= hi)
                || (span.hi >= lo && span.hi <= hi)
            {
                Some((lo, s))
            } else {
                None
            };

            lo = hi + 1;

            res
        })
        .collect::<Vec<_>>();

    if lines.len() > 1 {
        let mut msg = String::new();

        for (_, line) in lines {
            msg += "> ";
            msg += line;
            msg += "\n";
        }

        msg += "\n";
        msg + &String::from(message)
    } else {
        let (lo, line) = lines[0];
        let mut msg = String::from(line);
        msg += "\n";

        let start = span.lo - lo;

        msg += &line[..start]
            .chars()
            .map(|c| if c.is_whitespace() { c } else { ' ' })
            .collect::<String>();

        msg += &vec!['^'; span.hi - span.lo].into_iter().collect::<String>();

        msg += "\n";

        msg += message;

        msg += "\n";

        msg
    }
}
