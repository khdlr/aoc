set variable input_file = 'inputs/01';

create table input as select * from read_csv(getvariable(input_file), delim=' ', header=false, names=['left_val', 'drop', 'drop2', 'right_val']);

-- Create Ordered Lists
create table left_vals as select row_number() over (order by left_val asc) as idx, left_val  from input;
create table right_vals as select row_number() over (order by right_val asc) as idx, right_val  from input;

-- Part 1
select sum(abs(left_val - right_val)) as part1_solution from left_vals, right_vals where right_vals.idx = left_vals.idx;

-- Part 2
create table part2_counts as select left_val, left_val * count(*) as count from left_vals inner join right_vals on left_val = right_val group by left_val, left_vals.idx;
select sum(count) as part2_solution from part2_counts;
