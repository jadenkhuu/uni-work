-- COMP3311 22T3 Final Exam Q2
-- List of races with only Mares

-- put helper views (if any) here

-- answer: Q2(name,course,date)

create or replace view Q2(name,course,date)
as
... put sql here ...
select
  ra.name as name,
  rc.name as course,
  m.run_on as date
from
  horses h 
  join runners ru on ru.horse = h.id
  join races ra on ra.id = ru.race
  join meetings m on m.id = ra.part_of
  join racecourses rc on rc.id = m.run_at
where
;
