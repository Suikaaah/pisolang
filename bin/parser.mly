%{
  open Surface.MStr

  let rec nat_of_int_pat n =
    if n = 0 then PatCtor "Z"
             else PatApp ("S", nat_of_int_pat (n - 1))

  let rec nat_of_int_epat n =
    if n = 0 then EPatCtor "Z"
             else EPatCtorApp ("S", nat_of_int_epat (n - 1))

  let rec nat_of_int_term n =
    if n = 0 then TermCtor "Z"
             else TermCtorApp ("S", nat_of_int_term (n - 1))

  let rec list_of_ps ps =
    let folder p acc = PatApp ("Cons", PatTuple (List2.of_list [p; acc])) in
    List1.fold_right folder ps (PatCtor "Nil")

  let rec list_of_eps eps =
    let folder ep acc = EPatCtorApp ("Cons", EPatTuple (List2.of_list [ep; acc])) in
    List1.fold_right folder eps (EPatCtor "Nil")

  let rec list_of_terms terms =
    let folder t acc = TermCtorApp ("Cons", TermTuple (List2.of_list [t; acc])) in
    List1.fold_right folder terms (TermCtor "Nil")

  let rec lambdas_of_params = function
    | [] -> fun omega -> omega
    | psi :: tl -> fun omega -> IsoFun (psi, lambdas_of_params tl omega)
%}

%token EOF LPAREN RPAREN LBRACKET RBRACKET TIMES TRIANGLE PIPE COMMA SEMICOLON CONS
       ARROW BIARROW EQUAL UNIT LET IN FIX TYPE REC OF FUN CASE MATCH WITH
%token <int> NAT
%token <string> TICKED LOWER UPPER

%start <program> program
%type <typedef> typedef
%type <base> base_grouped base
%type <variant> variant
%type <pat> pat_grouped pat_almost pat
%type <epat> epat_grouped epat_almost epat_cons epat_triangle epat
%type <expr> expr expr_nocase
%type <pat * expr> branch
%type <iso> iso_grouped iso_almost iso_triangle iso
%type <term> term_grouped term_almost term_cons term_triangle term
%%

list1_impl(separator, X):
  | x = X; { [x] }
  | x = X; separator; xs = list1_impl(separator, X); { x :: xs }

list1(separator, X):
  | l = list1_impl(separator, X); { List1.of_list l }

list2_impl(separator, X):
  | x = X; separator; y = X; { [x; y] }
  | x = X; separator; xs = list2_impl(separator, X); { x :: xs }

list2(separator, X):
  | l = list2_impl(separator, X); { List2.of_list l }

program:
  | ts = typedef*; SEMICOLON; SEMICOLON; t = term; EOF;
    {
      let omega = IsoFun ("'f", IsoInv (IsoVar "'f")) in
      (ts, TermIso { phi = "'inv"; omega; t })
    }

typedef:
  | TYPE; name = LOWER; EQUAL; PIPE?; variants = list1(PIPE, variant);
    { { params = []; name; variants = List1.to_list variants } }

  | TYPE; param = TICKED; name = LOWER; EQUAL; PIPE?; variants = list1(PIPE, variant);
    { { params = [param]; name; variants = List1.to_list variants } }

  | TYPE; LPAREN; params = list2(COMMA, TICKED); RPAREN; name = LOWER;
    EQUAL; PIPE?; variants = list1(PIPE, variant);
    { { params = List2.to_list params; name; variants = List1.to_list variants } }

base_grouped:
  | LPAREN; a = base; RPAREN; { a }
  | UNIT; { BaseUnit }
  | x = LOWER; { BaseIdent x }
  | v = TICKED; { BaseVar v }
  | a = base_grouped; x = LOWER; { BaseApp (List1.of_list [a], x) }
  | LPAREN; aa = list2(COMMA, base); RPAREN; x = LOWER; { BaseApp (List2.to_list1 aa, x) }

base:
  | a = base_grouped; { a }
  | l = list2(TIMES, base_grouped); { BaseProd l }

variant:
  | c = UPPER; OF; a = base; { (c, Some a)}
  | c = UPPER; { (c, None) }

pat_grouped:
  | LPAREN; p = pat; RPAREN; { p }
  | LPAREN; RPAREN; { PatUnit }
  | LPAREN; l = list2(COMMA, pat); RPAREN; { PatTuple l }
  | x = LOWER; { PatVar x }
  | c = UPPER; { PatCtor c }
  | n = NAT; { nat_of_int_pat n }
  | LBRACKET; RBRACKET; { PatCtor "Nil" }
  | LBRACKET; ps = list1(SEMICOLON, pat); RBRACKET; { list_of_ps ps }

pat_almost:
  | p = pat_grouped; { p }
  | c = UPPER; p = pat_grouped; { PatApp (c, p) }

pat:
  | p = pat_almost; { p }
  | p_1 = pat_almost; CONS; p_2 = pat; { PatApp ("Cons", PatTuple (List2.of_list [p_1; p_2])) }

epat_grouped:
  | LPAREN; ep = epat; RPAREN; { ep }
  | LPAREN; RPAREN; { EPatUnit }
  | LPAREN; l = list2(COMMA, epat); RPAREN; { EPatTuple l }
  | x = LOWER; { EPatVar x }
  | c = UPPER; { EPatCtor c }
  | n = NAT; { nat_of_int_epat n }
  | LBRACKET; RBRACKET; { EPatCtor "Nil" }
  | LBRACKET; eps = list1(SEMICOLON, epat); RBRACKET; { list_of_eps eps }

epat_almost:
  | ep = epat_grouped; { ep }
  | c = UPPER; ep = epat_grouped; { EPatCtorApp (c, ep) }
  | omega = iso_almost; ep = epat_grouped; { EPatIsoApp (omega, ep) }

epat_cons:
  | ep = epat_almost; { ep }
  | ep_1 = epat_almost; CONS; ep_2 = epat_cons;
    { EPatCtorApp ("Cons", EPatTuple (List2.of_list [ep_1; ep_2])) }

epat_triangle:
  | ep = epat_cons; { ep }
  | ep = epat_triangle; TRIANGLE; omega = iso_almost; { EPatIsoApp (omega, ep) }

epat:
  | ep = epat_triangle; { ep }
  | MATCH; e = epat; WITH; PIPE?; l = list1(PIPE, branch);
    { EPatIsoApp (IsoCase l, e) }

expr:
  | ep = epat_triangle; { ExprEPat ep }
  | LET; p = pat; EQUAL; ep = epat; IN; e = expr;
  | LPAREN; LET; p = pat; EQUAL; ep = epat; IN; e = expr_nocase; RPAREN;
    { ExprLet { p; ep; e } }

expr_nocase:
  | ep = epat; { ExprEPat ep }
  | LET; p = pat; EQUAL; ep = epat; IN; e = expr_nocase;
  | LPAREN; LET; p = pat; EQUAL; ep = epat; IN; e = expr_nocase; RPAREN;
    { ExprLet { p; ep; e } }

branch:
  | p = pat; BIARROW; e = expr; { (p, e) }

iso_grouped:
  | LPAREN; omega = iso; RPAREN; { omega }
  | phi = TICKED; { IsoVar phi }

iso_almost:
  | omega = iso_grouped; { omega }
  | omega_1 = iso_almost; omega_2 = iso_grouped; { IsoApp (omega_1, omega_2) }

iso_triangle:
  | omega = iso_almost; { omega }
  | omega_2 = iso_triangle; TRIANGLE; omega_1 = iso_almost; { IsoApp (omega_1, omega_2) }

iso:
  | omega = iso_triangle; { omega }
  | CASE; PIPE?; l = list1(PIPE, branch); { IsoCase l }
  | FIX; phi = TICKED; ARROW; omega = iso; { IsoFix (phi, omega) }
  | FUN; params = TICKED+; ARROW; omega = iso; { lambdas_of_params params omega }

term_grouped:
  | LPAREN; t = term; RPAREN; { t }
  | LPAREN; RPAREN; { TermUnit }
  | LPAREN; l = list2(COMMA, term); RPAREN; { TermTuple l }
  | x = LOWER; { TermVar x }
  | c = UPPER; { TermCtor c }
  | n = NAT; { nat_of_int_term n }
  | LBRACKET; RBRACKET; { TermCtor "Nil" }
  | LBRACKET; l = list1(SEMICOLON, term); RBRACKET; { list_of_terms l }

term_almost:
  | t = term_grouped; { t }
  | c = UPPER; t = term_grouped; { TermCtorApp (c, t) }
  | omega = iso_almost; t = term_grouped; { TermIsoApp (omega, t) }

term_cons:
  | t = term_almost; { t }
  | t_1 = term_almost; CONS; t_2 = term_cons;
    { TermCtorApp ("Cons", TermTuple (List2.of_list [t_1; t_2])) }

term_triangle:
  | t = term_cons; { t }
  | t = term_triangle; TRIANGLE; omega = iso_almost; { TermIsoApp (omega, t) }

term:
  | t = term_triangle; { t }
  | LET; p = pat; EQUAL; t_1 = term; IN; t_2 = term; { TermLet { p; t_1; t_2 } }
  | LET; phi = TICKED; params = TICKED*; EQUAL; omega = iso; IN; t = term;
    { TermIso { phi; omega = lambdas_of_params params omega; t } }

  | LET; phi = TICKED; params = TICKED*; p = pat; EQUAL; e = expr_nocase; IN; t = term;
    {
      let omega = IsoCase List1.((p, e) :: []) in
      TermIso { phi; omega = lambdas_of_params params omega; t }
    }

  | MATCH; t = term; WITH; PIPE?; l = list1(PIPE, branch); { TermIsoApp (IsoCase l, t) }
  | LET; REC; phi = TICKED; params = TICKED*; EQUAL; omega = iso; IN; t = term;
    {
      let omega = lambdas_of_params params omega in
      TermIso { phi; omega = IsoFix (phi, omega); t }
    }

  | LET; REC; phi = TICKED; params = TICKED*; p = pat; EQUAL; e = expr_nocase; IN; t = term;
    {
      let omega = IsoCase List1.((p, e) :: []) in
      let omega = lambdas_of_params params omega in
      TermIso { phi; omega = IsoFix (phi, omega); t }
    }
