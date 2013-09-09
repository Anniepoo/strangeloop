:- module(mailman, []).
/** <module> demo handler of mailman

The mailman facility allows us to generate html whenever
it makes sense for the business logic, not when it comes
up in the html generation

*/
% Needed for handler definitions
:- use_module(library(http/http_dispatch)).

% Needed to generate html
:- use_module(library(http/html_write)).

:- http_handler(root(mailman), mailman_handler, []).

mailman_handler(_Request) :-
	reply_html_page(
	    title('mailman demo'),
	    [
		h1('Things Move To Where They Need'),
		\html_receive(topstuff),
		p('Some stuff'),
		p('More stuff'),
		p('Yet More stuff'),
		p('Even More stuff'),
		\where_we_figure_out_the_info
	    ]).

where_we_figure_out_the_info -->
	{
	    % only now do we have what we need

        },
	html([
	    \html_post(topstuff,
		       [p('This will be at the top')])
	]).


%
% There is an implied html_receives(head) in the head
% which is quite useful for adding to the head. For css
% and js use the resource system
