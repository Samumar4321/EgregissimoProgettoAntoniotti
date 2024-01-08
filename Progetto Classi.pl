%%%% -*- Mode: Prolog -*-


:- dynamic(class/3).
:- dynamic(class_field/4).
:- dynamic(instance/3).
:- dynamic(class_method/4).

%%% getFirst(List, FirstElement)
%%%
%%% List � una lista qualunque,
%%% FirstElement � un valore che corrisponde al primo elemento delle lista
%%%
%%% Permette di prendere il primo elemento di una lista,
%%% o controllare che un elemento sia il primo di una lista
getFirst([],[]) :- !.
getFirst([N],N) :- !.
getFirst([N | Ns], N) :- !.


%%% check_type(Value, Type)
%%%
%%% Value deve essere un valore del tipo Type
%%% Type � il tipo del parametro value
%%%
%%% La funzione permette di verificare che un valore sia di tipo specificato
%%% oppure di recuperare un il tipo di un valore
check_type(Value, Class_Name) :-
    atom(Value),
    is_class(Class_Name),
    !.
check_type(Value, Class_Name) :-
    atom(Value),
    var(Class_Name),
    inst(Value, R),
    instance(Value, Class_Name, _) = R,
    !.
check_type(Value, Class_Name) :-
    is_list(Value),
    is_class(Class_Name),
    check_type_list(Value, Class_Name),
    !.
check_type(Value, integer) :- 
    integer(Value),
    !.
check_type(Value, float) :- 
    float(Value),
    !.
check_type(Value, number) :- 
    number(Value),
    !.
check_type(Value, string) :- 
    string(Value),
    !.
check_type(Value, integer) :-
    is_list(Value),
    check_type_list(Value, integer),
    !.
check_type(Value, float) :-
    is_list(Value),
    check_type_list(Value, float),
    !.
check_type(Value, number) :-
    is_list(Value),
    check_type_list(Value, number),
    !.
check_type(Value, string) :-
    is_list(Value),
    check_type_list(Value, string),
    !.


%%% check_type_list(List, Type)
%%%
%%% List � una lista qualsiasi
%%% Type � il tipo degli elementi della lista
%%%
%%% Funzione di appongio di check_type che
%%% permette a quest'ultima di funzionare con le liste
check_type_list([], Type) :- !.
check_type_list([V | Vs], Type) :-
    check_type(V, Type),
    !,
    check_type_list(Vs, Type).


%%% get_parents(Class_Name, Result)
%%%
%%% Class_Name � un atomo che definisce il nome di una classe
%%% Result � una lista contenente tutti i parents della classe Class_Name
%%%
%%% Mi permette di ottenere tutti i parents di una classe,
%%% compresi quelli ereditati da altre classi, sotto forma di lista
get_parents(Class_Name, Result) :-
    class(Class_Name, Parents, _),   
    get_parents_app(Parents, Result),
    !.


%%% get_parents_app(List, Result)
%%%
%%% List � una lista dinamica che contiene dei parents
%%% Result � una lista che contiene i tutti i parents
%%% recuperati partendo da una lista di parents iniziale
%%%
%%% Funzione di appoggio per get_parents che mi permette di
%%% recuperare tutti i parents partendo da una lista dinamica iniziale
get_parents_app([],[]):- 
    !.
get_parents_app([P], [P | Result]):-
    class(P, Result, _),
    !.
get_parents_app([P | Ps], Result) :-
    class(P, Parents, _),
    append(Ps, Parents, Conc_Parents),
    get_parents_app(Conc_Parents, Mid_Result),
    append([P], Mid_Result, Result),
    !.


%%% replace_word(Sentence, Word_to_Replace, New_Word, New_Sentence)
%%%
%%% Sentence � la frase iniziale da cui voglio sostituire una parola
%%% Word_To_Replace � la parola che voglio sostituire nella Sentence
%%% New_Word � la parola che sostituir� Word_To_Replace nella Sentence
%%% New_Sentence � la frase con Word_To_Replace sostituit�
%%%
%%% Funzione che mi permette di prendere una frase contenente una parola
%%% e andarla a sostituire con un'altra parola
%%% mantenendo intatta la struttura della frase
replace_word(Sentence, Word_to_Replace, New_Word, New_Sentence) :-
    call_nth(sub_atom(Sentence, B_Char, _, A_Char, Word_to_Replace), 1),
    sub_atom(Sentence, 0, B_Char, _, Before), 
    sub_atom(Sentence, _, A_Char, 0, After), 
    atomic_list_concat([Before, New_Word, After], Result),
    replace_word(Result, Word_to_Replace, New_Word, New_Sentence), 
    !.
replace_word(Sentence, _, _, Sentence).


%%% get_parents_methods(Class_Name, Result)
%%%
%%% Class_Name atomo che indica il nome di una classe definita
%%% Result lista contenente i metodi dei parents della classe Class-Name
%%%
%%% Recupera i metodi dei parents di una classe passata come parametro
get_parents_methods(Class_Name, Result) :-
    class(Class_Name, Parents, _),  
    get_methods(Parents, Result).


%%% get_methods(List, Result)
%%%
%%% List � una lista di atomi che corrispondono a nomi di classi
%%% Result lista contenente tutti i metodi che appartengono alle classi di List
%%%
%%% La funzione costruisce tramite ricorsione una lista
%%% contenente tutti i metodi della lista di classi passata come parametro.
%%% La lista 
get_methods([],[]) :-
    !.
get_methods([Class_Name], Result) :-
    is_class(Class_Name),   
    class(Class_Name, _, Parts),    
    keep_methods(Parts, Result),
    !.
get_methods([C | Cs], Result) :-
    is_class(C),
    get_methods(Cs, Mid_Result),
    class(C, _, Parts),
    keep_methods(Parts, Result),
    !.


keep_methods([], []).
keep_methods([method(Name, Args, Body) | Ps], Result):-
    keep_methods(Ps, R),
    append([method(Name, Args, Body)], R, Result).
keep_methods([field(_,_,_) | Ps], Result):-
    keep_methods(Ps, R),
    append([], R, Result).
    

def_class(Name, Parents) :-
    def_class(Name, Parents, []).
def_class(Name, Parents, Parts) :-
    (\+ class(Name, _, _)),
    atom(Name),
    !,
    is_list(Parents),
    maplist(is_class, Parents),
    is_list(Parts),  
    assert(class(Name, Parents, [])),
    get_parents_methods(Name, Parents_Methods),
    append(Parts, Parents_Methods, Mid_Parts),
    maplist(check_part(Name), Mid_Parts),
    findall(method(Methods_Name, Args, Body),
	    class_method(Name, Methods_Name, Args, Body), Methods),
    write(Methods), 
    copy_fields(Name, Temp1),
    inherit_fields(Parents, Temp2),
    append(Temp2, Temp1, Temp3),
    remove_field_duplicates(Temp3, Fields), 
    retract(class(Name, Parents,[])),
    append(Fields, Methods, Final_Parts),
    assert(class(Name, Parents, Final_Parts)).
/*def_class(Name, Parents, Parts) :-
    class(Name, _, _),
    retract(class(Name, _, _)),
    !.*/


is_class(Name) :-
    atom(Name),
    class(Name, _, _),  
    !.


is_instance(Value):- 
    atom(Value),
    instance(Value, _, _),
    !.
is_instance(Value, Class_Name) :-
    atom(Value),
    is_class(Class_Name),
    instance(Value, N, _),
    is_superclass_of(Class_Name,N),
    !.
is_instance(Value, Class_Name) :-
    atom(Value),
    is_class(Class_Name),
    instance(Value, Class_Name, _),
    !.
is_instance(Value):-
    var(Value),
    call(instance(Value, _, _)).
is_instance(Value, Class_Name):-
    var(Value),
    is_class(Class_Name),
    call(instance(Value, Class_Name, _)).
is_instance(Value):-
    compound(Value),
    call(Value).


is_superclass_of(Super_Class, Class):-
    findall([Class, Parents, Parts], class(Class, Parents, Parts), Classes),
    getFirst(Classes,[Name, Parents, Parts]),
    is_superclass_of(Super_Class, Class, Parents).
is_superclass_of(Super_Class, Class, []) :- fail.
is_superclass_of(Super_Class, Class, [P | Ps]) :-    
    Super_Class \= P,
    findall([P, Parents, Parts], class(P, Parents, Parts), Classes),
    getFirst(Classes,[Name, Parents, Parts]),
    append(Ps, Parents, Result),
    is_superclass_of(Super_Class,Class, Result),
    !.  
is_superclass_of(Super_Class, Class, [P | Ps]) :-
    Super_Class =@= P,
    true.


inst(Instance_Name, Instance) :-
    atom(Instance_Name),
    instance(Instance_Name, Class_Name, Fields),
    Instance = instance(Instance_Name, Class_Name, Fields).


field(Instance, Field_Name, Result):-
    atom(Instance),
    atom(Field_Name),
    instance(Instance, _, Z),
    member(Field_Name = Result, Z),
    !.  
field(Instance, Field_Name, Result):-
    compound(Instance),
    atom(Field_Name),
    instance(Instance_Name, Class_Name, Z) = Instance,
    member(Field_Name = Result, Z),
    !.  


fieldx(Instance, [FN], Result):- 
    atom(FN),
    field(Instance, FN, Result),
    !.
fieldx(Instance, [FN | FNs], Result):-    
    atom(FN),
    field(Instance, FN, Res),
    is_instance(Res),
    fieldx(Res, FNs, Result),
    !.


check_part(Class_Name, field(Field_Name, Field_Value)) :-
    check_part(Class_Name, field(Field_Name, Field_Value, T)),
    !.
check_part(Class_Name, field(Field_Name, Field_Value, Field_Type)) :-
    is_class(Class_Name),
    atom(Field_Name),
    (\+ class_field(Class_Name, Field_Name, _, _)),
    check_type(Field_Value, Field_Type),
    !,
    assert(class_field(Class_Name, Field_Name, Field_Value, Field_Type)).
check_part(Class_Name, method(Method_Name, Args, Body)) :-
    is_class(Class_Name),
    atom(Method_Name),
    is_list(Args),
    (\+ class_method(Class_Name, Method_Name, _, _)),
    install_method(Class_Name, Method_Name, Args, Body),
    assert(class_method(Class_Name, Method_Name, Args, Body)),
    !.
check_part(Class_Name, method(Method_Name, Args, Body)) :-
    is_class(Class_Name),
    atom(Method_Name),
    is_list(Args),
    class_method(Class_Name, Method_Name, _, _),
    !.

/*
check_exist_method(Method_List, Method_Name, Num_Args) :-  
    check_length(Method_List, Num_Args),
    !.


check_length([[Method_Name, Args] | Ms], Arg_Length) :-
    length(Args, L),
    Arg_Length = L,
    !.
check_length([[Method_Name, Args] | Ms], Arg_Length) :-
    length(Args, L),
    Arg_Length =\= L,
    check_length(Ms, Arg_Length),
    !.
*/

install_method(Class_Name, Method_Name, Args, Body) :-    
    This_Args = [this | Args],  
    append([Method_Name], This_Args, Method_Head),    
    Head =.. Method_Head,   
    term_to_atom(Head, Atom_Head), 
    term_to_atom(Body, Atom_Body),    
    Check = (instance(this, C, _), C =@= Class_Name),
    term_to_atom(Check, Atom_Check),    
    atom_concat(Atom_Check, ", ", Final_Atom_Check),
    atom_concat(Final_Atom_Check, Atom_Body, Checked_Atom_Body),
    atom_concat(Atom_Head, " :- ", Complete_Atom_Head), 
    atom_concat(Checked_Atom_Body, ", !.", Complete_Atom_Body),
    atom_concat(Complete_Atom_Head,
		Complete_Atom_Body,
		Complete_Method_No_This),
    atom_string(Complete_Method_No_This, String_Method),
    replace_word(String_Method, "this", "This", Complete_Atom_Method),  
    term_to_atom(Complete_Method, Complete_Atom_Method),
    assert(Complete_Method).


make(Instance_Name, Class_Name):-
    make(Instance_Name,Class_Name,[]).
make(Instance_Name, Class_Name, To_Modify_Fields) :-
    atom(Instance_Name),  
    (\+ instance(Instance_Name, _, _)),
    is_class(Class_Name),  
    copy_fields(Class_Name, Class_Fields),
    findall([Class_Name, Parents, Parts], 
        class(Class_Name, Parents, Parts), 
        Classes),
    getFirst(Classes,[Name, Parents, Parts]),
    inherit_fields(Parents, Parents_Fields),
    append(Parents_Fields, Class_Fields, Fields),
    remove_field_duplicates(Fields, Fields_No_Dup),
    refactor_to_instance_fields(Fields_No_Dup, Instance_Fields),   
    sort(Instance_Fields, Sorted_Fields_No_Dup),
    sort(To_Modify_Fields, Sorted_To_Modify_Fields),
    modify_fields(Class_Name,
		  Sorted_Fields_No_Dup,
		  Sorted_To_Modify_Fields,
		  Result),
    assert(instance(Instance_Name, Class_Name, Result)),
    !.
make(Instance_Name, Class_Name, To_Modify_Fields) :-
    var(Instance_Name), 
    is_class(Class_Name),
    is_list(To_Modify_Fields), 
    copy_fields(Class_Name, Class_Fields), 
    findall([Class_Name, Parents, Parts], 
        class(Class_Name, Parents, Parts), 
        Classes),
    getFirst(Classes,[Name, Parents, Parts]),
    inherit_fields(Parents, Parents_Fields), 
    append(Parents_Fields, Class_Fields, Fields),
    remove_field_duplicates(Fields, Fields_No_Dup),
    refactor_to_instance_fields(Fields_No_Dup, Instance_Fields),    
    sort(Instance_Fields, Sorted_Fields_No_Dup),  
    sort(To_Modify_Fields, Sorted_To_Modify_Fields),
    modify_fields(Class_Name,
		  Sorted_Fields_No_Dup,
		  Sorted_To_Modify_Fields,
		  Result),
    Instance_Name = instance(generic, Class_Name, Result).
make(Instance_Name, Class_Name, To_Modify_Fields) :-
    var(Instance_Name), 
    is_class(Class_Name),
    is_list(To_Modify_Fields),
    is_class(Class_Name),  
    is_instance(Instance_Name, Class_Name). 


remove_field_duplicates([L],[L]):-  
    !.    
remove_field_duplicates([L1, L2 | Ls], R):-
    field(N, V, T) = L1,
    member(field(N, _, _), Ls), 
    !,
    remove_field_duplicates([L2 | Ls], R).
remove_field_duplicates([L1, L2 | Ls], R):-
    field(N, V, T) = L1,
    (\+ member(field(N, _, _), Ls)), 
    !,      
    remove_field_duplicates([L2 | Ls], R1),
    append(R1, [L1], R).


inherit_fields([], []) :-
    !.
inherit_fields(P, Parents_Fields) :-
    copy_fields(P, Parents_Fields).     
inherit_fields([P | Ps], Parents_Fields) :-    
    findall([P, Parents, Parts], class(P, Parents, Parts), Classes),
    getFirst(Classes,[Name, Parents, Parts]),
    append(Ps, Parents, Result),
    inherit_fields(Result, IFs),
    inherit_fields(P, Instance_Fields),
    append(Instance_Fields, IFs, Parents_Fields),
    !. 


copy_fields(Class_Name, Instance_Fields) :-
    class(Class_Name,_,_),    
    findall(field(N, V, T), class_field(Class_Name, N, V, T), Class_Fields),   
    copy_assert(Class_Fields, Instance_Fields),
    !.


copy_assert([],[]) :- !.
copy_assert([F], Temp):-
    field(N, V, T) = F,
    Temp = [F].
copy_assert([F | Fs], Instance_Fields) :-
    copy_assert( Fs, IFs),
    field(N, V, T) = F,
    Temp = [F],
    append(IFs, Temp, Instance_Fields).


modify_fields(Class_Name, [], [], []) :-  
    !.
modify_fields(Class_Name, [IF | IFs], [], Result):-
    append([IF], IFs, Result),
    !.
modify_fields(Class_Name, [IF | IFs], [TMF | TMFs], Result) :-
    [N = V] = [IF],
    [MN = MV] = [TMF],
    MN =@= N,
    !,
    class(Class_Name, _, Parts),
    member(field(N, V, T), Parts),
    !,
    check_type(MV, New_Type),
    T = New_Type,
    modify_fields(Class_Name, IFs, TMFs, R1),
    append([N = MV], R1, Result).    
modify_fields(Class_Name, [IF | IFs], [TMF | TMFs], Result) :-
    [N = V] = [IF],
    [MN = MV] = [TMF],
    MN \= N,
    !,
    modify_fields(Class_Name, IFs, [TMF | TMFs], R1),
    append([N = V], R1, Result).


refactor_to_instance_fields([], []) :-
    !.
refactor_to_instance_fields([F | Fs], R) :-
    refactor_to_instance_fields(Fs, R1),
    field(N, V, T) = F,
    append([N = V], R1, R),
    !.

/* DEBUG */
initialize():-
    call(def_class(a,[],[field(a,"",string),field(zampe,16,integer),
                            method(writeName,[Name],(field(this,a,Name),write(Name))),
                            method(writeHello,[],(write("\n\n####-HELLO-####\n\n")))])),
    call(def_class(b,[a],[field(b,"",string),field(zampe,60,integer)])),
    call(def_class(animale,[b],[field(specie,"",string),field(zampe,0,integer)])),
    call(def_class(persona,[b],[field(nome,"persona",string),field(cognome,"",string)])),
    call(def_class(cane,[persona,animale],[field(padrone, vivvio, persona),field(nome,"",string),field(anni,0,integer), 
                            method(writeName,[Name],(field(this,nome,Name),write(Name)))])),
   
    !.

/* END DEBUG */
