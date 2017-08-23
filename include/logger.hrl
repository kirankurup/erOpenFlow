%%%-------------------------------------------------------------------
%%% @author Kiran N
%%% @copyright (C) 2017,
%%% @doc
%%%
%%% @end
%%% Created : 31. Mar 2017 12:42
%%%-------------------------------------------------------------------
-author("kirankurup@gmail.com").

-ifndef (_LOGGER_HRL).
-define (_LOGGER_HRL, true).

-define (LOG_DEBUG(Fmt, Args),        lager:debug(Fmt, Args)).
-define (LOG_INFO(Fmt, Args),         lager:info(Fmt, Args)).
-define (LOG_WARN(Fmt, Args),         lager:warning(Fmt, Args)).
-define (LOG_ERROR(Fmt, Args),        lager:error(Fmt, Args)).

-endif.
