# cspmf 
A front-end library for parsing CSP-M specifications. It includes also an occurrence check analysis and it provides options for translating the AST to Prolog and XML. Written in Haskell, the library uses the Parsec parser combinator of Haskell. The library provides a command 'cspmf', which comprises different options enabling the user to parse CSP-M specifications and translate the respective AST into Prolog terms or XML-files. There are also options for parsing single CSP expressions and declarations from command line in the context of a CSP-M file.

This library is used in the ProB tool for parsing CSP-M specifications. For more information on the ProB tool visit the homepage of the tool: https://www3.hhu.de/stups/prob/index.php/Main_Page
 
The library includes three packages:
  - CSPM-Frontend: The package provides functions for lexing, parsing, renaming and pretty-printing CSP-M specifications.
  - CSPM-ToProlog: The package provides functions for translating a CSP-M AST into Prolog (mainly interesting for developers of the ProB tool).
  - CSPM-cspm-frontend: The package that provides the command line tool 'cspmf'.

# Install 'cspmf'
To intsall 'cspmf' just execute 'cabal_sandbox_install.sh' from CSPM-cspm-frontend. The shell script installs all necessary Haskell libraries and packages needed to build the 'cspmf' command; it uses the cabal sandboxes enabling to build the cspmf command in isolation. Ensure to have at least GHC 7.10.3 and the Cabal package installed. The 'cspmf' binary is generated in 'dist/build/cspmf/' folder, which is located in CSPM-cspm-frontend.

It is also possible to install the packages using Cabal:
   cabal install CSPM-*
In case you use the cabal installation note that CSPM-cspm-frontend depends on building CSPM-Frontend and CSPM-ToProlog first. Additionally, CSPM-ToProlog requires installing CSPM-Frontend.

Tested with GHC-7.10.3.

# Usage of 'cspmf'
To see the available options execute 'cspmf --help' from the command line.

To parse a specification and write the parse result and the AST to a file use the following options:

    cspm translate [OPTIONS] FILE
       --rename                          run renaming  on the AST
       --xmlOut=FILE                     optional: write a file with containing XML
       --prettyOut=FILE                  optional: prettyPrint to a file
       --addUnicode=FILE                 optional: replace some CSPM symbols with unicode
       --removeUnicode=FILE              optional: replace some unicode symbols with default CSPM encoding
       --prologOut=FILE                  translate a CSP-M file to Prolog
       --expressionToPrologTerm=STRING   translate a single CSP-M expression to Prolog
       --declarationToPrologTerm=STRING  translate a single CSP-M declaration to Prolog

Remark: Currently one can translate single expressions and declarations only into Prolog.
