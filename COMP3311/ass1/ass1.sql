-- COMP3311 23T1 Assignment 1

-- Q1: amount of alcohol in the best beers

-- put any Q1 helper views/functions here

create or replace view Q1(beer, "sold in", alcohol)
as
select 
	name as beer, 
	concat(concat(volume, 'ml '), sold_in)  as "sold in", 
	concat(round(cast(volume * abv / 100 as numeric), 1), 'ml')  as "alcohol" 
from 
	beers 
where 
	rating > 9
;

-- Q2: beers that don't fit the ABV style guidelines

-- put any Q2 helper views/functions here

create or replace view Q2lesser
as
select
	b.name as "name",
	s.name as "style",
	b.abv as "abv",
	concat('too weak by ', cast(s.min_abv - b.abv as numeric(4,1)), '%') as "reason"
	
from
	beers b,
	styles s
where
	b.style = s.id and
	b.abv < s.min_abv
;

create or replace view Q2greater
as
select 
	b.name as "name",
	s.name as "style",
	b.abv as "abv",
	concat('too strong by ', cast(b.abv - s.max_abv as numeric(4,1)), '%') as "reason"
from 
	beers b,
	styles s
where 
	b.style = s.id and
	b.abv > s.max_abv
;

create or replace view Q2(beer, style, abv, reason)
as
select 
	*
from
	Q2lesser
UNION
select 
	*
from
	Q2greater
;

-- select null, null, null::ABVvalue, null  -- replace this with your SQL code
-- ;

-- Q3: Number of beers brewed in each country

-- put any Q3 helper views/functions here

create or replace view Q3(country, "#beers")
as
select
	countries.name as "country",
	count(beers.id) as "#beers"
from
	countries 
	left join
	locations on countries.id = locations.within 
	left join
	Breweries on locations.id = Breweries.located_in 
	left join
	brewed_by on Breweries.id = brewed_by.brewery 
	left join
	beers on brewed_by.beer = beers.id
group by
	countries.name
;

-- Q4: Countries where the worst beers are brewed

-- put any Q4 helper views/functions here

create or replace view Q4(beer, brewery, country)
as
select
	b.name as "beer",
	bw.name as "brewery",
	c.name as "country"
from
	countries c
	left join
	locations l on c.id = l.within
	left join
	Breweries bw on l.id = bw.located_in
	left join
	brewed_by bb on bw.id = bb.brewery
	left join
	beers b on bb.beer = b.id
where
	b.rating < 3
;

-- select null, null, null  -- replace this with your SQL code

-- Q5: Beers that use ingredients from the Czech Republic

-- put any Q5 helper views/functions here

create or replace view Q5(beer, ingredient, "type")
as
select
	b.name as "beer",
	i.name as "ingredient",
	i.itype as "type"
from 
	countries c
	left join
	ingredients i on c.id = i.origin,
	contains cn
	left join
	beers b	on cn.beer = b.id
where
	i.origin = 31 and -- 31 is country id of CZE
	cn.ingredient = i.id
;
-- select null, null, null::IngredientType  -- replace this with your SQL code

-- Q6: Beers containing the most used hop and the most used grain

-- put any Q6 helper views/functions here
create or replace view Q6hop
as
select
	i.id as "id",
	i.itype	as "type",
	count(b.id) as "num"
from
	ingredients i
	left join
	contains c on i.id = c.ingredient
	left join
	beers b on c.beer = b.id
where
	i.itype = 'hop'
group by
	i.id, i.itype
order by num desc
limit 1
;

create or replace view Q6grain
as
select
	i.id as "id",
	i.itype	as "type",
	count(b.id) as "num"
from
	ingredients i
	left join
	contains c on i.id = c.ingredient
	left join
	beers b on c.beer = b.id
where
	i.itype = 'grain'
group by
	i.id, i.itype
order by num desc
limit 1
;

create or replace view Q6(beer)
as
select
	b.name as "beer"
from
	contains c
	left join
	beers b on c.beer = b.id,
	Q6hop
where
	c.ingredient = Q6hop.id

INTERSECT

select
	b.name as "beer"
from
	contains c
	left join
	beers b on c.beer = b.id,
	Q6grain
where
	c.ingredient = Q6grain.id
;

 
-- null  -- replace this with your SQL code

-- Q7: Breweries that make no beer

-- put any Q7 helper views/functions here
create or replace view Q7helper
as
select
	breweries.name as "breweries",
	count(beers.id) as "nbeers"
from
	Breweries
	left join
	brewed_by on Breweries.id = brewed_by.brewery 
	left join
	beers on brewed_by.beer = beers.id
group by
	Breweries.name
;

create or replace view Q7(brewery)
as
select 
	breweries as "brewery"
from
	Q7helper
where
	Q7helper.nbeers = 0
;

-- replace this with your SQL code

-- Q8: Function to give "full name" of beer

-- put any Q8 helper views/functions here

create or replace function
	Q8(beer_id integer) returns text
as
$$
declare
	res text := '';
	brewName text;
begin
	perform * from beers where beers.id = beer_id;
	if (not found) then
		return 'No such beer';
	end if;

	for brewName in
		select regexp_replace(bw.name, ' (Beer|Brew).*$', '')
		from breweries bw, brewed_by bb
		where bb.beer = beer_id and bb.brewery = bw.id
	loop
		if (res = '') then
			res := res || brewName;
		else
			res := res || ' + ' || brewName;
		end if;
	end loop;

	res := res || ' ' || (select beers.name from beers where beers.id = beer_id);
	return res;
end;
$$ language plpgsql
;


-- Q9: Beer data based on partial match of beer name
-- put any Q9 helper views/functions here

drop type if exists BeerData cascade;
create type BeerData as (beer text, brewer text, info text);

create or replace function
	Q9(partial_name text) returns setof BeerData
as
$$
declare
	res BeerData;
	hopInfo text;
	grainInfo text;
	extrasInfo text;
	curr beers%rowtype;

begin
	for curr in 
		select b.id, b.name 
		from beers b 
		where b.name ilike '%' || partial_name || '%' 
	loop
		res.beer = curr.name;
		
		select (
			select
				string_agg(bw.name, ' + ' order by bw.name) 
			from 
				brewed_by bb	
				join
				breweries bw on bb.brewery = bw.id
			where bb.beer = curr.id
		) into res.brewer;

		select (
			select
				'Hops: ' || string_agg(i.name, ',' order by i.name)
			from
				contains c
				join 
				ingredients i on c.ingredient = i.id
			where
				i.itype = 'hop'
				and
				c.beer = curr.id
		)	into hopInfo;

		select (
			select
				'Grain: ' || string_agg(i.name, ',' order by i.name)
			from
				contains c
				join 
				ingredients i on c.ingredient = i.id
			where
				i.itype = 'grain'
				and
				c.beer = curr.id
		) into grainInfo;

		select (
			select	
				'Extras: ' || string_agg(i.name, ',' order by i.name)
			from
				contains c
				join
				ingredients i on c.ingredient = i.id
			where
				i.itype = 'adjunct'
				and
				c.beer = curr.id
		) into extrasInfo;

		if (hopInfo = '') then
			hopInfo = null;
		end if;

		if (grainInfo = '') then
			grainInfo = null;
		end if;

		if (extrasInfo = '') then
			extrasInfo = null;
		end if;

		res.info = concat_ws(E'\n', hopInfo, grainInfo, extrasInfo);

		return next res;
		end loop;
	return;
end;
$$ language plpgsql
;

