use std::collections::BTreeMap;
use std::error::Error;
use std::{env, fs::read_to_string};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
struct Extent {
    offset: usize,
    size: usize,
    file_id: usize,
}

fn print_extents(extents: &BTreeMap<usize, Extent>) {
    let mut len = 0;
    for (_, e) in extents {
        len = len.max(e.size + e.offset);
    }
    let symbols: Vec<char> = "0123456789abcdefghijklmnopqrstuvwxyz".chars().collect();
    let mut chrs: Vec<char> = (0..len).map(|_| '.').collect();
    for extent in extents.values() {
        let symbol = symbols[extent.file_id as usize % symbols.len()];
        for pos in extent.offset..(extent.offset + extent.size) {
            assert!(
                chrs[pos as usize] == '.',
                "Double char at {pos}! {} vs. {symbol}",
                chrs[pos as usize]
            );
            chrs[pos as usize] = symbol;
        }
    }
    println!("{}", chrs.iter().collect::<String>());
}

fn fragment(extents: &mut BTreeMap<usize, Extent>, offset: usize) {
    // Fragment! Assumes everything before `offset` is already fragmented
    // Step 1: Check that `offset` is free
    let mut offset = offset;
    let mut offset_changed = true;
    while offset_changed {
        offset_changed = false;
        if let Some((_, ext)) = extents.range(..=offset).last() {
            let old_offset = offset;
            offset = ext.offset + ext.size;
            offset_changed = old_offset != offset;
        }
    }
    // print_extents(extents);

    let (_, last_extent) = extents.pop_last().expect("No file in tree!");

    if offset >= last_extent.offset {
        extents.insert(last_extent.offset, last_extent);
        return;
    }

    let available_space = match extents.range((offset + 1)..).next() {
        Some((of, _)) => of - offset,
        None => usize::MAX,
    };

    let moved_size = last_extent.size.min(available_space);
    assert!(moved_size > 0);
    extents.insert(
        offset,
        Extent {
            offset,
            size: moved_size,
            file_id: last_extent.file_id,
        },
    );
    if last_extent.size > moved_size {
        extents.insert(
            last_extent.offset,
            Extent {
                offset: last_extent.offset,
                size: last_extent.size - moved_size,
                file_id: last_extent.file_id,
            },
        );
    }
    fragment(extents, offset + moved_size);
}

fn defragment(
    files_by_id: &mut BTreeMap<usize, Extent>,
    free_extents: &mut BTreeMap<usize, Extent>,
    file_id: usize,
) {
    // Fragment! Assumes everything before `offset` is already fragmented
    let last_extent = *files_by_id.get(&file_id).expect("File not in tree!");
    let first_fit = free_extents
        .iter()
        .filter(|(_, ext)| (ext.size >= last_extent.size) && (ext.offset < last_extent.offset))
        .next();

    if let Some((&offset, &free_extent)) = first_fit {
        // println!("Moving {file_id}...");
        // println!("{}v", (0..offset).map(|_| ' ').collect::<String>());
        files_by_id.insert(
            file_id,
            Extent {
                offset,
                size: last_extent.size,
                file_id: last_extent.file_id,
            },
        );
        free_extents.remove(&offset);
        if free_extent.size > last_extent.size {
            let free_offset = offset + last_extent.size;
            free_extents.insert(
                free_offset,
                Extent {
                    offset: free_offset,
                    size: free_extent.size - last_extent.size,
                    file_id: free_extent.file_id,
                },
            );
        }
    };

    // print_extents(&files_by_id);
    if file_id > 0 {
        defragment(files_by_id, free_extents, file_id - 1);
    }
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let input = read_to_string(&args[1])?;
    let line = input.lines().next().expect("No line in the input");

    let mut chars = line.chars();
    let mut extents: BTreeMap<usize, Extent> = Default::default();
    let mut free_extents: BTreeMap<usize, Extent> = Default::default();
    let mut offset = 0;
    let mut file_id = 0;

    while let Some(fs_char) = (&mut chars).next() {
        let file_size = fs_char.to_digit(10).expect("Couldn't parse char") as usize;
        extents.insert(
            offset,
            Extent {
                offset,
                size: file_size,
                file_id,
            },
        );
        file_id += 1;
        offset += file_size;
        if let Some(free_space_char) = (&mut chars).next() {
            let free_space = free_space_char.to_digit(10).expect("Couldn't parse char") as usize;
            free_extents.insert(
                offset,
                Extent {
                    offset,
                    size: free_space,
                    file_id: usize::MAX,
                },
            );
            offset += free_space;
        }
    }

    let mut part1_extents = extents.clone();
    fragment(&mut part1_extents, 0);

    let mut checksum: u64 = 0;
    for ex in part1_extents.values() {
        for pos in ex.offset..(ex.offset + ex.size) {
            checksum += ex.file_id as u64 * pos as u64;
        }
    }

    println!("Part 1: {checksum}");

    let mut files_by_id: BTreeMap<usize, Extent> =
        extents.into_iter().map(|(_, e)| (e.file_id, e)).collect();
    defragment(&mut files_by_id, &mut free_extents, file_id - 1);
    // print_extents(&extents);

    let mut checksum: u64 = 0;
    for ex in files_by_id.values() {
        for pos in ex.offset..(ex.offset + ex.size) {
            checksum += ex.file_id as u64 * pos as u64;
        }
    }
    println!("Part 2: {}", checksum);

    Ok(())
}
