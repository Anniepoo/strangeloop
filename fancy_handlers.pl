:- module(fancy_handlers, []).
/** <module> Some fancier handlers

*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

% we'll need to fiddle with abstract locations
:- use_module(library(http/http_path)).

% and we need file support
:- use_module(library(http/http_files)).

% This handler has an id, a reference independent of it's
% abstract or physical URI path
% These are useful for creating links using location_by_id in
% termerized html
%
%  So now we can refer to the physical URI path, the abstract path,
%  the id, or the predicate name (zippy_handler in this case) in
%  location_by_id
%
:- http_handler(root(zip), zippy_handler, [id(zippy)]).

%%	zippy_handler(+Request:request) is det
%
%	handler for demoing ids
%
zippy_handler(_Request) :-
	reply_html_page(
	    title('I\'m zippy'),
	    p('I\'m zippy')
	).

%
% In a real application, we'd do this in strangeloop.pl
%
% Add an abstract URI path root to serve files from
%
% this is NOT a file location, it's an URI path
http:location(files, '/f', []).

%
% Now we want an abstract *file path* - not a URI, but
% an abstract name for a location, as when we say
% use_module(library(blah)).
%
user:file_search_path(static_files, assets).

% and now we're ready to serve files in a directory
:- http_handler(files(.),    % abstract name of URI path
		serve_files,
		[prefix]).   % prefix option on handler so we handle every
			     % URI which starts /f/

%%	serve_files(+Request:request) is det
%
%	Serve a static file in response to request
%
serve_files(Request) :-
		             % library method to serve static files,
		             % root location specified by abstract file
		             % path
	http_reply_from_files(static_files(.), [], Request).

%
% What if the files' not there?
serve_files(Request) :-
      http_404([], Request).

/*
      Challenge - how would you make a 'site offline' function?

*/
