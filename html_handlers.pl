:- module(html_handlers, [an_inclusion//0]).
/** <module> Handlers that use the built in html generation

*/

% Needed for handler definitions
:- use_module(library(http/http_dispatch)).

% Needed to generate html
:- use_module(library(http/html_write)).

% and we need the borders inclusion
:- use_module(fancy_borders).


%
% this handler uses an abstract path. The absolute
% paths we've been using are not good practice
:- http_handler(root(pagedemo), page_demo , []).

%%	page_demo(+Request:request) is det
%
%	A simple page using the 'termerized html' form
%
page_demo(_Request) :-
	reply_html_page(
	    title('Strangeloop Rocks!'),
	    div([h1('Strangeloop Rocks'),
		 p('This page was generated from termerized html')
		])).

/*
	Much going on above.

	reply_html_page/2 takes two pieces of *termerized HTML*
	One for the head, one for the body

	Termerized html represesents tags as compound terms.
If

*/

:- http_handler(root(lotsofsyntax), lots_of_syntax , []).

%%	lots_of_syntax(+Request:request) is det
%
%	handler that demonstrates lot of the termerized html
lots_of_syntax(_Request) :-
	reply_html_page(
	    title('Lots of Syntax'),
	    \syntax_demo).  % this is an inclusion

%%	syntax_demo is det
%
%	DCG that generates some examples of the termerized HTML
%	syntax
%
syntax_demo -->
	{
            Foo = 3,
	    Name = 'Anne Ogborn',
            Age = 28,
            MyMessage = 'howdy there'
        },
	html([
	    p('tag with simple contents'),
	    p(['tag with ', b('compound'), ' contents']),
	    p([style('color: #ff0000'), title('rollover text')],
	      'arity 2 tags have attributes'),
	    '<b>this doesn\'t generate tags</b>',
	    p(['An entity ', &(copy), ' like that']),
	    \['<p>literal inclusion (rather evil)</p>'],
	    p('format style ~w string'-[Foo]),
	    p(a(href('mep.php?'+[name=Name, age=Age]), 'A link')),
	    p(a(href('http://example.com/foo.php?msg='+encode(MyMessage)),
	      'you can encode anything with encode')),
	    p(a(href(location_by_id(zippy)),
		'This is a link to a handler with an id (yet to be covered)')),
	    p(class([bar, baz, mep]), 'this has 3 classes'),
	    \an_inclusion,
	    \fancy_border(dot_madness, \an_inclusion),
	    \an_inclusion_with_qq
	]).

%%     an_inclusion is det
%
%   The html//1 isn't needed if we're concatenating other inclusions
%
%   Note that this is exported
an_inclusion -->
	inclusion_a,
	inclusion_b.

%%	inclusion_a is det
%
%	just another sample
%	This time we need the html//1
%	because we're expressing termerized html
%
inclusion_a -->
	html(p('inclusion a')).
inclusion_b -->
	html(p('inclusion b')).

%%	an_inclusion_with_qq is det
%
%       Yet another way to handle HTML
%	generation
%
%	the html quasiquote syntax
%
an_inclusion_with_qq -->
	{
           Foo = mep
        },
	html([
	    p('the next para is from quasiquotes'),
	    html({|html(Foo)||<p>Foo</p>|})
	]).


