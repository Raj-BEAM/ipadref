-module(create_dist_tables).

-export([init_tables/0, insert_user/3, insert_project/2]).

-record(user, {
       id,
       name
      }).

-record(project, {
       title,
       description
      }).

-record(contributor, {
       user_id,
       project_title
      }).

init_tables() ->
 mnesia:create_table(user,
    [{ram_copies, ['enode1@lappie.rajbeam.net', 'enode2@vm1.rajbeam.net']},
     {attributes, record_info(fields, user)}]),
 mnesia:create_table(project,
    [{ram_copies, ['enode1@lappie.rajbeam.net', 'enode2@vm1.rajbeam.net']},
     {attributes, record_info(fields, project)}]),
 mnesia:create_table(contributor,
    [{ram_copies, ['enode1@lappie.rajbeam.net', 'enode2@vm1.rajbeam.net']},
     {type, bag}, {attributes, record_info(fields, contributor)}]).

insert_user(Id, Name, ProjectTitles) when ProjectTitles =/= [] ->
 User = #user{id = Id, name = Name},
 Fun = fun() ->
         mnesia:write(User),
         lists:foreach(
           fun(Title) ->
             [#project{title = Title}] = mnesia:read(project, Title),
             mnesia:write(#contributor{user_id = Id,
                                       project_title = Title})
           end,
           ProjectTitles)
       end,
 mnesia:transaction(Fun).

insert_project(Title, Description) ->
 mnesia:dirty_write(#project{title = Title,
                             description = Description}).
