:- module(simple_handlers, []).
/** <module> Some simple handlers

*/

:- use_module(library(http/http_dispatch)).

% define a handler for the path /
% note that this is an absolute path.
% Set the priority high so we override system defined
% root handlers
:- http_handler(/,                        % for the URI path /
		say_something('Hello'),   % query say_something('Hello', Request)
		[priority(100)]).         % at priority 100

% We can reuse our handler code and make a handler that says goodbye
:- http_handler('/goodbye', say_something('Goodbye'), []).

%%	say_something(+What:atom, +Request:request) is det
%
%	Handle uris that say something world
%
% this is the simplest, crudest way to handle a request.
% The request body is bound to current_input and output
% written to current_output will go out as httpresponse.
% notice that we need to write the headers ourselves
%
%       @arg What what to say
%	@arg Request the request object
%
say_something(What, _Request) :-
        format('Content-type: text/plain~n~n'),
        format('~w World!~n', [What]).

%
% Because we're not fighting the language, we can
% handle special situations. Lets define hello
% handlers for lots of languages

howdy('nei* ho', 'Cantonese').
howdy('ni hao',  'Mandarin').
howdy('hallo', 'Dutch').
howdy('hello', 'English').
howdy('howdy', 'Kansan').
howdy('salut', 'French').
howdy('guten tag', 'German').
howdy('shalom', 'Hebrew').
howdy('ciào', 'Italian').
howdy('coi', 'Lojban').

%
% Generate handlers like /hello/German
%
howdy_handlers :-
	howdy(Howdy, Lang),
	www_form_encode(Lang, URLLang),
	format(atom(Path), '/hello/~w', [URLLang]),
	http_handler(Path, say_something(Howdy), []),
	fail.
howdy_handlers.

:- howdy_handlers.

