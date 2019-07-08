:- module(get_ci_status, [], []).

:- use_module(library(process)).
:- use_module(library(pillow/json)).
:- use_module(library(format), [format/2]).
:- use_module(library(lists), [member/2]).
:- use_module(library(write), [writeq/1]).

:- export(run/0).
run :-
	Accept = 'Accept: application/vnd.travis-ci.2+json',
	URL = 'https://api.travis-ci.org/repos/ciao-lang/ciao/builds',
	process_call(path(curl), ['-s', '-H', Accept, URL], [stdout(string(_Xs))]),
	string_to_json(_Xs, _Y),
	_Y = json(_A),
	member(builds=_Bs, _A),
	member(json(_B), _Bs),
	member(commits=Commits, _A),
	member(state=string(State), _B),
	!,
	( member(json(C), Commits),
	  member(author_name=string(Author), C),
	  member(author_email=string(Email), C),
	  member(committed_at=string(Date), C),
	  member(sha=string(SHA), C),
	  format("~s ~s ~s <~s>~n", [SHA, Date, Author, Email]),
	  fail
	; true
	),
%	writeq(Commits), nl,
	format("state: ~s~n", [State]).
