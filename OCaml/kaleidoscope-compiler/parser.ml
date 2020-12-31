
let binop_precedence:(char, int) Hashtbl.t = Hashtbl.create 10

let precedence c = try Hashtbl.find binop_precedence c with Not_found -> -1

let rec parse_primary = parser
  | [< 'Token.Number n >] -> Ast.Number n
  | [< 'Token.Kwd '('; e=parse_expr; 'Token.Kwd ')' ?? "expected ')'" >] -> e
  | [< 'Token.Ident id; stream >] ->
      let rec parse_args accumulator = parser
        | [< e=parse_expr; stream >] ->
            begin parser
              | [< 'Token.Kwd ','; e=parse_args (e :: accumulator) >] -> e
              | [< >] -> e :: accumulator
            end stream
        | [< >] -> accumulator
      in
      let rec parse_ident id = parser
        | [< 'Token.Kwd '(';
            args=parse_args [];
            'Token.Kwd ')' ?? "expected ')'" >] ->
              Ast.Call (id, Array.of_list (List.rev args))
        | [< >] -> Ast.Variable id
      in
      parse_ident id stream
  | [< >] -> raise (Stream.Error "unknown token when expecting an expression."

and parse_expr = parser
  | [< lhs=parse_primary; stream >] -> parse_bin_rhs 0 lhs stream

and parse_bin_rhs expr_prec lhs stream =
  match Stream.peek stream with
  | Some (Token.Kwd c) when Hashtbl.mem binop_precedence c ->
      let token_prec = precedence c in
      if token_prec < expr_prec then lhs else begin
        Stream.junk stream;
        let rhs = parse_primary stream in
        let rhs =
          match Stream.peek stream with
