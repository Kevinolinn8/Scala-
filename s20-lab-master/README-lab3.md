continue building an interpreter for a simple subset of JavaScript. The
following is the grammar for this language. Notice that we will now
add support for *immutable variables*, *conditionals*, and a *console
print operator*:

- **program** *p* ::= *e* | `const` *x* `=` *e* `;` *p*

- **expression** *e* ::= *x* | *v* | *uop* *e* | *e* *bop* *e*
                | *e* `?` *e* `:` *e* | `console.log` `(` *e* `)`

- **value** *v* ::= *n* | *b* | `undefined`

- **unary operator** *uop* ::= `-` | `!`

- **binary operator** *bop* ::= `+` | `-` | `*` | `/` | `===` | `!==` | `<` | `<=` | `>` | `>=` | `&&` | `||`

- **identifier** *x*

- **number (float)** *n*

- **boolean** *b* ::= `true` | `false`
