-- COMP3311 22T3 Final Exam Q1
-- Horse(s) that have won the most Group 1 races

-- put helper views (if any) here
-- answer: Q1(horse)

create or replace view Q1(horse) 
as
select 
  h.name as horse
from 
  Horses h 
  join Runners rn on rn.horse = h.id 
  join Races r on r.id = rn.race
where 
  rn.finished = 1 
  and 
  r.level = 1 
group by h.name
having count(*) = (
  select count(*) from Horses h 
  join Runners rn on rn.horse = h.id 
  join Races r on r.id = rn.race
  where rn.finished = 1 and r.level = 1 
  group by h.name 
  order by count(*) desc
  limit 1
)
  order by count(*) desc;

;


-- ... put sql here ..
