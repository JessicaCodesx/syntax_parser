/**
 * Jessica Garcia
 * COSC3127
 * Syntax Verifier
 * May 2025
 * Dr. Townsend
 * 
 * Program reads an entire source file into memory, tokenizes it,
 * then uses recursive descent to verify that the token sequence conforms to
 * the grammar.  It reports if the syntax is valid or invalid
 * It uses recursive descent parsing to validate if the source file follows the grammar rules defined below
*/

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

/**
 * Syntax verifier defined by the grammar
 *
 *   <program>            → program begin <statement_list> end
 *                           - a program starts with "program begin", followed by statements, and ends with "end"
 *   <statement_list>     → <statement> { ";" <statement> }
 *                           - a list of statements separated by semicolons
 *   <statement>          → <assignment> | <if_statement> | <loop_statement>
 *                           - a statement can be an assignment, an if statement, or a loop statement
 *   <assignment>         → <variable> "=" <expression>
 *                           - an assignment assigns an expression to a variable
 *   <variable>           → identifier
 *                           - a variable is simply an identifier (a name)
 *   <expression>         → <term> { ("+" | "-") <term> }
 *                           - an expression is one or more terms connected by "+" or "-" operators
 *   <term>               → <factor> { ("*" | "/") <factor> }
 *                           - a term is one or more factors connected by "*" or "/" operators
 *   <factor>             → identifier | integer_constant | "(" <expression> ")"
 *                           - a factir cab ve a variable, a number, or a parenthesized expression
 *   <if_statement>       → if "(" <logic_expression> ")" then <statement>
 *                           - an if statement checks a condition and executes a statement if true
 *   <logic_expression>   → <variable> ("<" | ">") <variable>
 *                           - a logic expression compares two variables using "<" or ">" operators
 *   <loop_statement>     → loop "(" <logic_expression> ")" <statement>
 *                           - a loop statement repeatedly executes a statement while a condition is true
 */

public class SyntaxVerifier {
    /**
     * Token categories recognized by the lexer
     * defines all possible token types that can appear in the language
     */
    enum TokenType {
        // keywords
        PROGRAM, BEGIN, END, IF, THEN, LOOP,
        // identifiers and literals
        IDENTIFIER, INTEGER_LITERAL,
        // operators
        ASSIGN, PLUS, MINUS, MULTIPLY, DIVIDE, LESS_THAN, GREATER_THAN,
        // punctuation
        LEFT_PAREN, RIGHT_PAREN, SEMICOLON,
        // end of file
        EOF
    }

    /** 
     * Token represents a single lexical unit in the source code
     * Each token has a type (from TokenType enum) and the original text from the source
     */

    static class Token {
        final TokenType type; // category of this token
        final String text; // original text from the source code


        /** Constructs a new token with the specified type and text*/
        Token(TokenType type, String text) {
            this.type = type;
            this.text = text;
        }

        /** String representation of the token for debugging */
        @Override
        public String toString() {
            return type + "('" + text + " ')";
        }
    }

    /**
     * The lexer (or scanner) converts the source text into a sequence of tokens
     * It reads chars one by one and groups them into lexical units (tokens)
     */
    static class Lexer {
        private final String input; // the complete source text
        private int position = 0; // current position in the input string
        private char currentChar; // current character being processed
        private Token pushedBack = null; // for token lookahead (pushing a token back)

        /** constructs a new lexer to tokenize the given source code*/
        Lexer(String source){
            input = source;
            // initialize currentChar to the first character of the input, or null if empty
            currentChar = input.length() > 0 ? input.charAt(0) : '\0'; 
        }

        /** advance to next character, update currentChar, or null if past the end*/
        private void advance() {
            position++;
            // sets currentChar to the next char or null if at the end
            currentChar = position < input.length() ? input.charAt(position) : '\0';
        }

        /**skip spaces tabs and newlines*/
        private void skipWhitespace() {
            while (Character.isWhitespace(currentChar)) {
                advance();
            }
        }

        /** push one token back to be returned on the next nextToken() call */
        void pushBack(Token token) {
            this.pushedBack = token;
        }

        /** return next token from input or eof */
        Token nextToken() {
            // handle push back - return it if theres one waiting
            if (pushedBack != null) {
                Token t = pushedBack;
                pushedBack = null;
                return t;
            } 
            // skip whitespace
            skipWhitespace();

            // check for end of input
            if (currentChar == '\0') {
                return new Token(TokenType.EOF, "");
            }

            // handle single character tokens ie operators and punctuation
            switch (currentChar) {
                case '+': advance(); return new Token(TokenType.PLUS,       "+");
                case '-': advance(); return new Token(TokenType.MINUS,      "-");
                case '*': advance(); return new Token(TokenType.MULTIPLY,   "*");
                case '/': advance(); return new Token(TokenType.DIVIDE,     "/");
                case '=': advance(); return new Token(TokenType.ASSIGN,     "=");
                case '<': advance(); return new Token(TokenType.LESS_THAN,  "<");
                case '>': advance(); return new Token(TokenType.GREATER_THAN, ">");
                case '(': advance(); return new Token(TokenType.LEFT_PAREN, "(");
                case ')': advance(); return new Token(TokenType.RIGHT_PAREN,")");
                case ';': advance(); return new Token(TokenType.SEMICOLON,  ";");
            }

            // handle identifiers and keyworeds (starting with a letter)
            if (Character.isLetter(currentChar)) {
                StringBuilder sb = new StringBuilder();
                // read entire identifier/keyword (letters & digits)
                while (Character.isLetterOrDigit(currentChar)) {
                    sb.append(currentChar);
                    advance();
                }
                String lexeme = sb.toString();

                // check if its a keyword, otherwise its an identifier
                switch (lexeme) {
                    case "program": return new Token(TokenType.PROGRAM, lexeme);
                    case "begin":   return new Token(TokenType.BEGIN,   lexeme);
                    case "end":     return new Token(TokenType.END,     lexeme);
                    case "if":      return new Token(TokenType.IF,      lexeme);
                    case "then":    return new Token(TokenType.THEN,    lexeme);
                    case "loop":    return new Token(TokenType.LOOP,    lexeme);
                    default:        return new Token(TokenType.IDENTIFIER, lexeme);
                }
            }

            // handle integer literals (sequence of digits)
            if (Character.isDigit(currentChar)) {
                StringBuilder sb = new StringBuilder();
                // read entire number
                while (Character.isDigit(currentChar)) {
                    sb.append(currentChar);
                    advance();
                }
                return new Token(TokenType.INTEGER_LITERAL, sb.toString());
            }
            // if nothing matched, its an unknonwn char
            throw new RuntimeException("Unknown character: " + currentChar);
        }
    }

    /**
     * Recursive descent parser: one method per nonterminal and throws exceptions when syntax errors are found
     */
    static class SyntaxParser {
        private final Lexer lexer; // the lexer providing tokesn
        private Token currentToken; // the current token being processed

        /** constructs a parser using the given lexer as its token source*/
        SyntaxParser(Lexer lexer) {
            this.lexer = lexer;
            // read the first token to start parsing
            currentToken = lexer.nextToken();
        }

        /** verifies that the current token is of the expected tpye and advances to the next token
         * if the current token doesnt match expected type, throws an exception
        */
        private void expect(TokenType expected) {
            if (currentToken.type == expected) {
                // if it matches, consume this token and move to the next
                currentToken = lexer.nextToken();
            } else {
                // if it doesnt match, thow exception
                throw new RuntimeException("Syntax error: expected " + expected + " but found " + currentToken);
            }
        }

       /** <program> -> program begin <statement_list> end EOF */ 
       void parseProgram() {
        expect(TokenType.PROGRAM); // must start with "program"
        expect(TokenType.BEGIN); // must be followed by "begin"
        parseStatementList(); // then a list of statements
        expect(TokenType.END); // must end with "end"
        if (currentToken.type != TokenType.EOF) {
            throw new RuntimeException("Syntax error: extra text at end of program"); // throw exception if not EOF
           }
        }

        /** <statement_list> -> <statement> { ";" <statement> } */
        void parseStatementList() {
            // parse the first statement
            parseStatement();
            // then parse any additional statements separated by semicolons
            while (currentToken.type == TokenType.SEMICOLON) {
                expect(TokenType.SEMICOLON);
                parseStatement();
            }
        }

        /** <statement> -> <assignment> | <if_statement> | <loop_statement> */
        void parseStatement() {
            switch (currentToken.type) {
                case IDENTIFIER:
                    // if it starts with an identifier, it must be an assignment
                    parseAssignment();
                    break;
                case IF:
                    // if it starts with "if", it must be an if statement
                    parseIfStatement();
                    break;
                case LOOP:
                    // if it starts with "loop", it must be a loop statement
                    parseLoopStatement();
                    break;
                default:
                    // if it doesnt match any of the above, throw an exception
                    throw new RuntimeException("Syntax error in statement: " + currentToken);
            }
        }

        /** <assignment> -> <variable> "=" <expression> */
       void parseAssignment() {
            expect(TokenType.IDENTIFIER); // assignment starts with a variable (identifier)
            expect(TokenType.ASSIGN); // followed by an equals sign
            parseExpression(); // then an expression
        }

        /** <variable> -> identifier */
        void parseVariable() {
            expect(TokenType.IDENTIFIER); // a variable is just an identifier
        }

        /** <expression> -> <term> { ("+" | "-") <term> } */
        void parseExpression() {
            // an expression starts with a term
            parseTerm();
            // and may be followed by additional terms with "+" or "-" operators
            while (currentToken.type == TokenType.PLUS || currentToken.type == TokenType.MINUS) {
                if (currentToken.type == TokenType.PLUS) {
                    expect(TokenType.PLUS);
                } else {
                    expect(TokenType.MINUS);
                }
                parseTerm();
            }
        }

        /** <term> → <factor> { ("*" | "/") <factor> } */
        void parseTerm() {
            // a term starts with a factor
            parseFactor();
            // and may be followed by additional factors with "*" or "/" operators
            while (currentToken.type == TokenType.MULTIPLY
                || currentToken.type == TokenType.DIVIDE) {
                // consume the operator
                expect(currentToken.type);
                // parse the next factor
                parseFactor();
            }
        }

        /** <factor> -> identifier | integer_constant | "(" <expression> ")" */
        void parseFactor() {
            switch (currentToken.type) {
                case IDENTIFIER:
                    // a factor can be an identifier (variable)
                    expect(TokenType.IDENTIFIER);
                    break;
                case INTEGER_LITERAL:
                    // or an integer literal (number)
                    expect(TokenType.INTEGER_LITERAL);
                    break;
                case LEFT_PAREN:
                    // or a parenthesized expression
                    expect(TokenType.LEFT_PAREN);
                    parseExpression();
                    expect(TokenType.RIGHT_PAREN);
                    break;
                default:
                    // if it doesnt match any of the above, throw an exception
                    throw new RuntimeException("Syntax error in factor: " + currentToken);
            }
        }

        /** <if_statement> -> if "(" <logic_expression> ")" then <statement> */
        void parseIfStatement() {
            expect(TokenType.IF); // if statement starts with "if"
            expect(TokenType.LEFT_PAREN); // followed by a left parenthesis
            parseLogicExpression(); // then a logic expression (the conidition)
            expect(TokenType.RIGHT_PAREN); // followed by a right parenthesis
            expect(TokenType.THEN); // then the "then" keyword
            parseStatement(); // finally a statement to execute if the condition is true
        }

        /** <logic_expression> -> <variable> ("<" | ">") <variable> */
        void parseLogicExpression() {
            // a logic expression starts with a variable 
            parseVariable();
            // followed by a comparison operator ("<" or ">")
            if (currentToken.type == TokenType.LESS_THAN
             || currentToken.type == TokenType.GREATER_THAN) {
                expect(currentToken.type);
            } else {
                throw new RuntimeException("Expected '<' or '>' in logic expression but found " + currentToken);
            }
            // and ends with another variable 
            parseVariable();
        }

        /** <loop_statement> -> loop "(" <logic_expression> ")" <statement> */
        void parseLoopStatement() {
            expect(TokenType.LOOP); // loop statement starts with "loop"
            expect(TokenType.LEFT_PAREN); // followed by a left parenthesis
            parseLogicExpression(); // then a logic expression (the condition)
            expect(TokenType.RIGHT_PAREN); // followed by a right parenthesis
            parseStatement(); // then the statemtnt to repeat
        }
    }

    // Main method:  read file, lex + parse, report result
    public static void main(String[] args) {
        // check for one command line argument (source file)
         if (args.length != 1) {
            System.err.println("Usage: java SyntaxVerifier <sourcefile>");
            System.exit(1);
        }

        String programSource;
        try {
            // read the entire source file into a string
            programSource = new String(Files.readAllBytes(Paths.get(args[0])));
        } catch (IOException e) {
            // handle any errors when reading file
            System.err.println("Error reading source file: " + e.getMessage());
            return;
        }

        // create a lexer to tokenize the source 
        Lexer lexer = new Lexer(programSource);
        // create a parser to validate the token sequence
        SyntaxParser parser = new SyntaxParser(lexer);

        try {
            // try to parse the program
            parser.parseProgram();
            // if no exceptions are thrown, syntax is valid
            System.out.println("Syntax Valid!!!!!!");
        } catch (RuntimeException e) {
            // if an exception is thrown, syntax is invalid
            System.out.println("Syntax Error :(");
        }
    }
}