:- dynamic(class/3).
:- dynamic(class_field/4).
:- dynamic(instance_field/4).


def_class(Name, Parents) :-
    (\+ class(Name, _, _)),
    atom(Name),
    !,
    is_list(Parents),
    assert(class(Name, Parents, [])),
    is_class(Name).


def_class(Name, Parents, Parts) :-
    (\+ class(Name, _, _)),
    atom(Name),
    !,
    is_list(Parents),
    is_list(Parts),   
    maplist(check_part(Name), Parts),
    assert(class(Name, Parents, Parts)).

%%%dove cazzo la metto sta merda!!!!!
is_class(Name) :-
    atom(Name),
    class(Name, _, _),
    findall([Name, Parents, Parts], class(Name, Parents, Parts), Class), 
    nth0(0, Class, [Name, Parents, Parts]),   
   /* length(Parents, N),
    writeln(N),   */
    maplist(is_class, Parents),
    !.

/*
field(Field_Name, Field_Value) :-
    atom(Field_Name),
    !.

field(Field_Name, Field_Value, Field_Type) :-
    atom(Field_Name),
    atom(Field_Type),
    !.

method(Method_Name, Args, Body) :-
    is_list(Args),
    atom(Method_Name),
    !.
*/
/*check_part(field(Field_Name, Field_Value)) :- */


check_part(Class_Name, field(Field_Name, Field_Value)) :-
   is_class(Class_Name),
   atom(Field_Name),
   (\+ class_field(Class_Name, Field_Name, _, _)),
   !,
   assert(class_field(Class_Name, Field_Name, Field_Value, string)).


check_part(Class_Name, field(Field_Name, Field_Value, Field_Type)) :-
    is_class(Class_Name),
    atom(Field_Name),
    atom(Field_Type),
    (\+ class_field(Class_Name, Field_Name, _, _)),
    !,
    assert(class_field(Class_Name, Field_Name, Field_Value, Field_Type)).


check_part(Class_Name, method(Method_Name, Args, Body)) :-
    is_class(Class_Name),
    atom(Method_Name),
    is_list(Args),
    assert(method(Method_Name, Args, Body)).


make(Instance_Name, Class_Name, Instance_Fields) :-
    atom(Instance_Name),
    is_class(Class_Name),
    copy_attr(Instance_Name, Instance_Fields, Class_Name),
    assert(inst(Instance_Name, Class_Name)),
    !.


make(Instance_Name, Class_Name) :-
    atom(Instance_Name),
    is_class(Class_Name),
    copy_attr(Instance_Name, Class_Name),
    assert(inst(Instance_Name, Class_Name)),
    !.


copy_attr(Instance_Name, Class_Name) :-
    findall([N, V, T], class_field(Class_Name, N, V, T), Class_Fields),
    sort(Class_Fields, Sorted_Class_Fields),
    copy_assert(Instance_Name, Sorted_Class_Fields).


copy_attr(Instance_Name, Instance_Fields, Class_Name) :-
    findall([N, V, T], class_field(Class_Name, N, V, T), Class_Fields),
    sort(Class_Fields, Sorted_Class_Fields),
    sort(Instance_Fields, Sorted_Istance_Fields),
    copy_assert(Instance_Name, Sorted_Class_Fields, Sorted_Istance_Fields).


copy_assert(_,[]) :- !.
copy_assert(Instance_Name, [F | Fs]) :-
    [N, V, T] = F,
    assert(instance_field(Instance_Name, N, V, T)),
    copy_assert(Instance_Name, Fs).


copy_assert(_,[],[]) :- !.
copy_assert(Instance_Name, [F | Fs], []) :-
    copy_assert(Instance_Name, [F | Fs]).
copy_assert(Instance_Name, [F | Fs], [NF | NFs]) :-
    [CFN, CFV, CFT] = F,
    [IFN, IFV] = NF,
    CFN =@= IFN,
    !,
    assert(instance_field(Instance_Name, IFN, IFV, CFT)),
    copy_assert(Instance_Name, Fs, NFs).
copy_assert(Instance_Name, [F | Fs],[NF | NFs]) :-
    [CFN, CFV, CFT] = F,
    [IFN, IFV] = NF,
    CFN \= IFN,
    !,
    assert(instance_field(Instance_Name, CFN, CFV, CFT)),
    copy_assert(Instance_Name, Fs, [NF | NFs]).
