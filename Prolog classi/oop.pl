%%%% -*- Mode: Prolog -*-

%%% oop.pl

:- dynamic(class/3).
:- dynamic(class_field/4).
:- dynamic(instance/3).
:- dynamic(class_method/4).


%%% getFirst(List, FirstElement)
%%%
%%% List --> una lista qualunque,
%%% FirstElement -->  un valore che corrisponde al primo elemento delle lista
%%%
%%% Permette di prendere il primo elemento di una lista,
%%% o controllare che un elemento sia il primo di una lista
getFirst([], []) :-
    !.
getFirst([N], N) :-
    !.
getFirst([N | Ns], N) :-
    !.


%%% check_type(Value, Type)
%%%
%%% Value --> deve essere un valore del tipo Type
%%% Type --> il tipo del parametro value
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
%%% List --> una lista qualsiasi
%%% Type --> il tipo degli elementi della lista
%%%
%%% Funzione di appongio di check_type che
%%% permette a quest'ultima di funzionare con le liste
check_type_list([], Type) :-
    !.
check_type_list([V | Vs], Type) :-
    check_type(V, Type),
    !,
    check_type_list(Vs, Type).


%%% get_parents(Class_Name, Result)
%%%
%%% Class_Name --> un atomo che definisce il nome di una classe
%%% Result --> una lista contenente tutti i parents della classe Class_Name
%%%
%%% Mi permette di ottenere tutti i parents di una classe,
%%% compresi quelli ereditati da altre classi, sotto forma di lista
get_parents(Class_Name, Result) :-
    class(Class_Name, Parents, _),   
    get_parents_app(Parents, Result),
    !.


%%% get_parents_app(List, Result)
%%%
%%% List -->  una lista dinamica che contiene dei parents
%%% Result -->  una lista che contiene i tutti i parents
%%%             recuperati partendo da una lista di parents iniziale
%%%
%%% Funzione di appoggio per get_parents che mi permette di
%%% recuperare tutti i parents partendo da una lista dinamica iniziale
get_parents_app([], []) :- 
    !.
get_parents_app([P], [P | Result]) :-
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
%%% Sentence --> la frase iniziale da cui voglio sostituire una parola
%%% Word_To_Replace --> la parola che voglio sostituire nella Sentence
%%% New_Word --> la parola che sostituir� Word_To_Replace nella Sentence
%%% New_Sentence --> la frase con Word_To_Replace sostituitita
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
%%% Class_Name --> atomo che indica il nome di una classe definita
%%% Result --> lista contenente i metodi dei parents della classe Class-Name
%%%
%%% Recupera i metodi dei parents di una classe passata come parametro
get_parents_methods(Class_Name, Result) :-
    class(Class_Name, Parents, _),  
    get_methods(Parents, Result).


%%% get_methods(List, Result)
%%%
%%% List --> una lista di atomi che corrispondono a nomi di classi
%%% Result --> lista contenente tutti i metodi
%%%            che appartengono alle classi di List
%%%
%%% La funzione costruisce tramite ricorsione una lista
%%% contenente tutti i metodi della lista di classi passata come parametro. 
get_methods([], []) :-
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
    keep_methods(Parts, This_Result),
    append(This_Result, Mid_Result, Result),
    !.


%%% keep_methods(List, Result)
%%%
%%% List --> lista che contiene un insieme di parti, siano field o metodi
%%% Result ---> lista che contiene solo le parti di forma
%%%             method(Name, Args, Body)
%%%
%%% Funione di appoggio per get_methods che prende le parti di una classe
%%% e mantiene solo i metodi scartando i fields
keep_methods([], []).
keep_methods([method(Name, Args, Body) | Ps], Result) :-
    keep_methods(Ps, R),
    append([method(Name, Args, Body)], R, Result).
keep_methods([field(_, _, _) | Ps], Result):-
    keep_methods(Ps, R),
    append([], R, Result).

    
%%% def_class(Name, Parents, Parts)
%%%
%%% Name --> atomo che rappresenta il nome della classe
%%% Parents --> lista che rappresenta i parents diretti di una classe
%%% Parts --> lista che contiene fields e metodi di una classe
%%%
%%% Funzione che permette di definire una nuova classe.
%%% Non permette la ridefinizione di una classe una volta definita
%%% senza rimuoverla manualmente.
%%% Controlla che Parents sia una lista di classi definite.
%%% Recupera i metodi delle sue superclassi e
%%% li appende alle Parts per poterli ereditare o ridefinire.
%%% Controlla che ogni elemento di Parts sia corretto in forma e valori.
%%% Recupera i fields delle superclassi e li appende ai suoi fields
%%% per poi rimuovere quelli gia' definiti.
def_class(Name, Parents) :-
    def_class(Name, Parents, []).
def_class(Name, Parents, Parts) :-
    (\+ class(Name, _, _)),
    atom(Name),
    !,
    is_list(Parents),
    is_class(Parents),
    is_list(Parts),  
    assert(class(Name, Parents, [])),
    get_parents_methods(Name, Parents_Methods),
    append(Parts, Parents_Methods, Mid_Parts),
    check_part(Name, Mid_Parts),
    findall(method(Methods_Name, Args, Body),
	    class_method(Name, Methods_Name, Args, Body), Methods), 
    copy_fields(Name, Temp1),
    inherit_fields(Parents, Temp2),
    append(Temp2, Temp1, Temp3),
    remove_field_duplicates(Temp3, Fields), 
    retract(class(Name, Parents, [])),
    append(Fields, Methods, Final_Parts),
    assert(class(Name, Parents, Final_Parts)).


%%% is_class(Name)
%%%
%%% Name --> atomo che rappresenta il nome della classe da controllore
%%%
%%% Dato un nome Name controlla l'esistenza di una classe con quel nome 
is_class(Name) :-
    atom(Name),
    class(Name, _, _),  
    !.
is_class([]) :-
    !.
is_class([Class | Cls]) :-
    is_class(Class),
    !,
    is_class(Cls),
    !.

%%% is_instance(Value)
%%%
%%% Value --> atomo che rappresenta il nome di una istanza
%%%
%%% Controlla che Value sia il nome di una istanza definita
is_instance(Value) :- 
    atom(Value),
    instance(Value, _, _),
    !.
%%% is_instance(Value, Class_Name)
%%%
%%% Value --> atomo che rappresenta il nome di una istanza
%%% Class_Name --> nome di una classe
%%%
%%% Variante di is_instance
%%% Controlla che Value sia il nome di una istanza definita e
%%% di classe che ha come superclasse Class_Name
is_instance(Value, Class_Name) :-
    atom(Value),
    is_class(Class_Name),
    instance(Value, N, _),
    is_superclass_of(Class_Name, N),
    !.
%%% is_instance(Value, Class_Name)
%%%
%%% Value --> atomo che rappresenta il nome di una istanza
%%% Class_Name --> nome di una classe
%%%
%%% Variante di is_instance
%%% Controlla che Value sia il nome di una istanza definita e
%%% di classe Class_Name
is_instance(Value, Class_Name) :-
    atom(Value),
    is_class(Class_Name),
    instance(Value, Class_Name, _),
    !.
%%% is_instance(Value)
%%%
%%% Value --> variabile libera
%%%
%%% Variante di is_instance
%%% Se Value e' una variabile libera
%%% ritorna il nome della prima istanza che unifica con Value
is_instance(Value) :-
    var(Value),
    call(instance(Value, _, _)).
%%% is_instance(Value, Class_Name)
%%%
%%% Value --> variabile libera
%%% Class_Name --> nome di una classe
%%%
%%% Variante di is_instance
%%% Se Value e' una variabile libera unificala con 
%%% il nome della prima istanza di classe Class_Name
is_instance(Value, Class_Name) :-
    var(Value),
    is_class(Class_Name),
    call(instance(Value, Class_Name, _)).
%%% is_instance(Value)
%%%
%%% Value --> oggetto compound che rappresenta una istanza
%%%
%%% Variante di is_instance
%%% Controlla che Value sia un compound che rappresenta una istanza
is_instance(Value) :-
    compound(Value),
    call(Value).


%%% is_superclass_of(Super_Class, Class)
%%%
%%% Super_Class --> atomo che rappresenta il nome di una classe
%%% Class --> atomo che rappresenta il nome di una classe
%%%
%%% Funzione che controlla se Super_Class e' una superclasse di Class,
%%% dove sia Super_Class che Class devono essere classi definite.
%%% Usa la ricorsione per scorrere l'albero di superclassi di una classe e
%%% se trova una classe il cui nome corrisponde a Super_Class ritorna True
is_superclass_of(Super_Class, Class) :-
    findall([Class, Parents, Parts], class(Class, Parents, Parts), Classes),
    getFirst(Classes, [Name, Parents, Parts]),
    is_superclass_of(Super_Class, Class, Parents).
is_superclass_of(Super_Class, Class, []) :-
    fail.
is_superclass_of(Super_Class, Class, [P | Ps]) :-    
    Super_Class \= P,
    findall([P, Parents, Parts], class(P, Parents, Parts), Classes),
    getFirst(Classes, [Name, Parents, Parts]),
    append(Ps, Parents, Result),
    is_superclass_of(Super_Class, Class, Result),
    !.  
is_superclass_of(Super_Class, Class, [P | Ps]) :-
    Super_Class =@= P,
    true.

%%% inst(Instance_Name, Instance)
%%%
%%% Instance_Name --> atomo che rappresenta il nome di una istanza
%%% Instance --> valore che corrisponde all'instanza di nome Instance_Name
%%%
%%% Dato un Instance_Name, nome di una istanza definita, se:
%%% 1)Instance e' una variabile libera allora unificala con
%%%   la rappresentazione dell'istanza;
%%% 2)Altrimenti controlla se Instance corrisponde con
%%%   la rappresentazione dell'instanza
inst(Instance_Name, Instance) :-
    atom(Instance_Name),
    instance(Instance_Name, Class_Name, Fields),
    Instance = instance(Instance_Name, Class_Name, Fields).


%%% field(Instance, Field_Name, Result)
%%%
%%% Instance --> rappresenta  una istanza
%%% Field_Name --> nome di un field presente nell'istanza
%%% Result --> valore del field con nome Field_Name nell'istanza Instance
%%%
%%% Dato Instance, rappresentazione di una istanza definita, se:
%%% 1)Instance e' un atomo allora corrisponde al nome di una istanza,
%%%   recupero il valore del field Field_Name e lo unifico con Result;
%%% 2)Altrimenti controlla se Instance corrisponde con
%%%   la rappresentazione di una instanza definita e
%%%   recupera il valore del field Field_Name e lo unifico con Result
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


%%% fieldx(Instance, List, Result)
%%%
%%% Instance --> rappresenta  una istanza
%%% List --> lista che rappresenta la catena di field da risalire
%%% Result --> valore del field che ha come nome l'ultimo elemento di List
%%%            nell'istanza Instance
%%%
%%% Recupera il valore del field che ha come nome l'ultimo elemento di List e
%%% unificalo con Result.
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


%%% check_part(Class_Name, List)
%%%
%%% Class_Name  --> rappresenta  una classe definita
%%% List --> lista che rappresenta un insieme di parti(fields o metodi)
%%%
%%% Funzione ricorsiva che analizza una lista di parts,
%%% seleziona la versione corretta in base
%%% al tipo di dato che si deve analizzare:
%%% -se si tratta di un fields, controlla che non sia stato gia' definito,
%%%  che sia di tipo corretto allora esegui la assert;
%%% -se si tratta di un method, controlla che non sia stato gia' definito,
%%%  procedi con la creazione del metodo effettivo e poi esegui assert
check_part(Class_Name, []) :-
    !.
check_part(Class_Name, [field(Field_Name, Field_Value) | Ps]) :-
    check_part(Class_Name, [field(Field_Name, Field_Value, T) | Ps]),
    !.
check_part(Class_Name, [field(Field_Name, Field_Value, Field_Type) | Ps]) :-
    is_class(Class_Name),
    atom(Field_Name),
    (\+ class_field(Class_Name, Field_Name, _, _)),
    check_type(Field_Value, Field_Type),
    !,
    assert(class_field(Class_Name, Field_Name, Field_Value, Field_Type)),
    check_part(Class_Name, Ps),
    !.
check_part(Class_Name, [method(Method_Name, Args, Body) | Ps]) :-
    is_class(Class_Name),
    atom(Method_Name),
    is_list(Args),
    (\+ class_method(Class_Name, Method_Name, _, _)),
    install_method(Class_Name, Method_Name, Args, Body),
    assert(class_method(Class_Name, Method_Name, Args, Body)),
    check_part(Class_Name, Ps),
    !.
check_part(Class_Name, [method(Method_Name, Args, Body) | Ps]) :-
    is_class(Class_Name),
    atom(Method_Name),
    is_list(Args),
    class_method(Class_Name, Method_Name, _, _),
    check_part(Class_Name, Ps),
    !.

%%% install_method(Class_Name, Method_Name, Args, Body)
%%%
%%% Class_Name --> rappresenta il nome di una classe
%%% Method_Name --> nome del metodo da "installare"
%%% Args --> lista di argomenti del motodo
%%% Body --> corpo del metodo
%%%
%%% Appendo alla lista Args in prima posizione l'atomo this,
%%% creo la testa del metodo,
%%% trasformo tutto in atomi per poterli modificare piu' facilmente.
%%% Aggiungo controlli che mi permettono di gestire
%%% la corretta chiamata dei metodi.
%%% Concateno la testa con il body e
%%% aggiungo il simbolo di cut alla fine del body.
%%% Esegui la funzione che dato un metodo con Head :- Body
%%% mi sostituisca la parola this con la parola This.
%%% Trasformo tutto il metodo in un termine ed eseguo la assert
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


%%% make(Instance_Name, Class_Name, List)
%%%
%%% Instance_Name --> nome dell'istanza da creare
%%% Class_Name --> tipo di classe dell'istanza da creare
%%% List --> opzionale, lista di field da modificare quando si crea l'istanza
%%%
%%% La funzione controlla che non esista gia' una istanza cn questo nome,
%%% procede con la copia dei fields base di Class_Name.
%%% Poi recupera i parents della classe e i loro fields,
%%% gli appende a quelli della classe e rimuove i fields ridefiniti.
%%% Dopodiche' procede a vedere se ci sono fields da modificare,
%%% specificati in List, se ci sono li modifica.
%%% Se tutta la procedura sopracitata non produce errori
%%% prosegue all'assert dell'istanza
make(Instance_Name, Class_Name) :-
    make(Instance_Name, Class_Name, []).
make(Instance_Name, Class_Name, To_Modify_Fields) :-
    atom(Instance_Name),  
    (\+ instance(Instance_Name, _, _)),
    is_class(Class_Name),  
    copy_fields(Class_Name, Class_Fields),
    findall([Class_Name, Parents, Parts], 
            class(Class_Name, Parents, Parts), 
            Classes),
    getFirst(Classes, [Name, Parents, Parts]),
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
%%% Variante di make che accetta come Instance_Name una variabile e
%%% restituisce l'istanza generica della classe Class_Name specificata.
%%% I fields vengono modificati come specificato in To_Modify_Fields
make(Instance_Name, Class_Name, To_Modify_Fields) :-
    var(Instance_Name), 
    is_class(Class_Name),
    is_list(To_Modify_Fields), 
    copy_fields(Class_Name, Class_Fields), 
    findall([Class_Name, Parents, Parts], 
            class(Class_Name, Parents, Parts), 
            Classes),
    getFirst(Classes, [Name, Parents, Parts]),
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
%%% Variante di make che accetta come Instance_Name una variabile e ritorna
%%% tutte le istanze della classe Class_Name.
%%% To_Modify_Fields � necessatio per il meccanismo di backtracking di Prolog,
%%% infatti quando si esegui la versione sopracitata della make si eseguira'
%%% anche questa, nel caso ci siano delle istanze
make(Instance_Name, Class_Name, To_Modify_Fields) :-
    var(Instance_Name), 
    is_class(Class_Name),
    is_list(To_Modify_Fields),
    is_class(Class_Name),  
    is_instance(Instance_Name, Class_Name). 


%%% remove_field_duplicates(List1, Result)
%%%
%%% List --> lista originale da cui rimuovere i duplicati
%%% Result --> lista ricostruita con i duplicati rimossi
%%%
%%% Funzione che data una lista di fields di forma [field(N, V, T), ...],
%%% elimina tutti i field che hanno lo stesso nome.
%%% L'eliminazione da' priorita' agli elementi finali eliminando 
%%% quindi le prime occorrenze di un field duplicato.
%%% Se un field non e' duplicato lo aggiungo alla lista di ritorno Result
remove_field_duplicates([L], [L]) :-  
    !.    
remove_field_duplicates([L1, L2 | Ls], R ) :-
    field(N, V, T) = L1,
    member(field(N, _, _), Ls), 
    !,
    remove_field_duplicates([L2 | Ls], R).
remove_field_duplicates([L1, L2 | Ls], R) :-
    field(N, V, T) = L1,
    (\+ member(field(N, _, _), Ls)), 
    !,      
    remove_field_duplicates([L2 | Ls], R1),
    append(R1, [L1], R).


%%% inherit_fields(List, Result)
%%%
%%% List --> lista che rappresenta le classi da cui estrarre i fields,
%%%          in genere dei parents
%%% Result --> lista che contiene tutti i fields delle classi in List
%%%
%%% Data una lista List che contiene delle classi,
%%% recupera i loro parents e aggiungili in fondo alla lista List,
%%% recupera poi i field della classe in esame e scorri la lista ricorsivamente.
inherit_fields([], []) :-
    !.
inherit_fields(P, Parents_Fields) :-
    copy_fields(P, Parents_Fields).     
inherit_fields([P | Ps], Parents_Fields) :-    
    findall([P, Parents, Parts], class(P, Parents, Parts), Classes),
    getFirst(Classes, [Name, Parents, Parts]),
    append(Ps, Parents, Result),
    inherit_fields(Result, Parent_Field),
    inherit_fields(P, IFs),
    append(IFs, Parent_Field, Parents_Fields),
    !. 


%%% copy_fields(Class_Name, List)
%%%
%%% Class_Name --> nome di una classe definita
%%% List --> lista contenente i fields di quella classe
%%%
%%% Data una classe definita, recupera i suoi fields
copy_fields(Class_Name, Result) :-
    class(Class_Name, _, _),   
    findall(field(N, V, T), class_field(Class_Name, N, V, T), Class_Fields),  
    Result = Class_Fields,
    !.


%%% modify_fields(Class_Name, Instance_Fields, New_Fields, Result)
%%%
%%% Class_Name --> nome della classe a cui una istanza appartiene
%%% Instance_Fields --> lista contenente i fields di una istanza
%%%                     con i valori di default
%%% New_Fields --> lista contenente i fields di cui voglio modificare
%%%                il valore e il nuovo valore
%%% Result --> lista contenente tutti i fields con valori modificati
%%%
%%% La funzione modifica il valore dei fields presenti in Istance_Fields
%%% con il valore delle corrispettivo fields in New_Fields.
%%% Analizza ogni elemento delle due liste,
%%% se su un elemento di Instance_Fields non vi e' una corrispondenza in
%%% New_Fields lo copia in Result con il valore di default.
%%% In caso contrario controlla che il nuovo valore sia di tipo corretto,
%%% se vero copia il fields in Result con il valore modificato
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

%%% refactor_to_instance_fields(List, Result)
%%%
%%% List --> lista di fields di forma (Name, Value, Type)
%%% Result --> lista dei fields di List in forma (Name, Value)
%%%
%%% Mi permette di prendere una lista di fields
%%% con elementi di forma (Name, Value, Type)
%%% e trasformarla in una lista con gli stessi fields ma di forma (Name, Value).
%%% Questa funzione e' utile per trasformare i fields di una classe
%%% in fields di una istanza
refactor_to_instance_fields([], []) :-
    !.
refactor_to_instance_fields([F | Fs], R) :-
    refactor_to_instance_fields(Fs, R1),
    field(N, V, T) = F,
    append([N = V], R1, R),
    !.

%%%% end of file -- oop.pl --