:- module(load, []).
/** <module> Load this to start the production environment

*/

% A bit of possibly excessive abstraction, we load the
% server but don't run it in strangeloop.pl
:-use_module(strangeloop).
% make sure the handlers get loaded
:- ensure_loaded(simple_handlers).
:- ensure_loaded(html_handlers).
:- ensure_loaded(fancy_handlers).
:- ensure_loaded(styling_handlers).
:- ensure_loaded(handle_params).
:- ensure_loaded(sessions).
:- ensure_loaded(mailman).
:- ensure_loaded(resourcedemo).
:- ensure_loaded(clippy_demo).
:- ensure_loaded(workshop).




