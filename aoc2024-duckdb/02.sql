set variable input_file = 'inputs/02';

create table raw_inputs as select * from read_csv(getvariable(input_file), delim=';', header=false, names=['vals']);
create table inputs as
  select list_transform(string_split(vals, ' '), x -> cast(x as integer)) as vals from raw_inputs;
create table differences as select list_transform(list_zip(vals[:-2], vals[2:]), x -> x[2] - x[1]) as diff from inputs;

create table safeness as
  select
    diff,
    len(list_filter(diff, x -> sign(x) != sign(diff[1]))) == 0 as all_same_sign,
    len(list_filter(diff, x -> (abs(x) < 1) or (abs(x) > 3))) == 0 as all_1_to_3,
    from differences;

select count(*) as part1 from safeness
  where all_same_sign and all_1_to_3;
