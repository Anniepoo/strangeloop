:- module(handle_params, []).
/** <module> demo of parameter conversion and validation

*/

%  needed to make termerized html
:- use_module(library(http/html_write)).
%  needed for http_handler
:- use_module(library(http/http_dispatch)).
%  needed to handle parameters
:- use_module(library(http/http_parameters)).

% a normal handler
:- http_handler(root(params), param_handler , []).

%
%  Try this with   (valid)
% http://localhost:7777/params?title=something&age=40&name=annie
%
% and with  (default)
%  http://localhost:7777/params?age=40&name=annie
%
% and with (invalid)
%  http://localhost:7777/params?age=400&name=annie
%

%%	param_handler(+Request:request) is det
%
%	this is a demo handler that validates it's arguments
%
%	* title an optional title for the page
%
%	* name  the users name
%
%	* age the users age
%
param_handler(Request) :-
        http_parameters(Request,
                        [ title(Title, [ default('The Default Title') ]),
                          name(Name,   [ length >= 2 ]),
                          age(Age,     [ between(0, 150) ])
                        ]),
	reply_html_page(
	    [title(Title)],
	    [
		p(['Your name is ', Name]),
		p(['Your age is ', Age])  %aside - notice it handles num/atom conversion
	    ]).


%
% in this one we'll cope with the invalid params
%
:- http_handler(root(perror), param_error , []).

%%	param_error(+Request:request) is det
%
%  this handler catches the error
%  This being Prolog, we can template match the error type using
%  unification, so we're not handling some other issue with page
%  generation, just the validation rule we want
%
param_error(Request) :-
	catch(param_handler(Request),
	      error(type_error(ValidationRule,Got),_),
	      reply_html_page(
		    [title('Oops')],
		    p('Invalid parameter, ~w fails validation rule ~w'-[Got, ValidationRule]))
		).

%
%  writing out the validation rules for all the parameters in all the
%  handlers when they're usually the same is donkey work. Lets do it
%  once

:- http_handler(root(peasy), params_once, []).

%%	params_once(+Request:request) is det
%
% the only new part is an option list at the end with
% attribute_declarations(param)
% This says 'you can get the validation rule for this
% from the param/2 fact
%
params_once(Request) :-
        http_parameters(Request,
                        [ title(Title),
                          name(Name),
                          age(Age)
                        ],
                        [ attribute_declarations(param)
                        ]),
	reply_html_page(
	    [title(Title)],
	    [
		p(['Your name is ', Name]),
		p(['Your age is ', Age])  %aside - notice it handles num/atom conversion
	    ]).

%
% these could be shared by many handlers
%
param(title, [optional(true)]).
param(name,  [length >= 2 ]).
param(age,   [integer]).

%
% Experiment: look up http_parameters in pldoc.
% It comes in two arities as we've seen
% http_parameters/3 has another option, form_data - see what that
% returns by modifying params_once

