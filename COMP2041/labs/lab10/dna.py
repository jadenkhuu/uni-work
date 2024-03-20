def read_dna(dna_file):
    """
    Read a DNA string from a file.
    the file contains data in the following format:
    A <-> T
    G <-> C
    G <-> C
    C <-> G
    G <-> C
    T <-> A
    Output a list of touples:
    [
        ('A', 'T'),
        ('G', 'C'),
        ('G', 'C'),
        ('C', 'G'),
        ('G', 'C'),
        ('T', 'A'),
    ]
    Where either (or both) elements in the string might be missing:
    <-> T
    G <->
    G <-> C
    <->
    <-> C
    T <-> A
    Output:
    [
        ('', 'T'),
        ('G', ''),
        ('G', 'C'),
        ('', ''),
        ('', 'C'),
        ('T', 'A'),
    ]
    """
    data = []
    with open(dna_file, 'r') as file:
        for line in file:
            line = line.strip()
            if line:
                if '<->' in line:
                    base1, base2 = line.split('<->')
                    data.append((base1.strip(), base2.strip()))

    return data

def is_rna(dna):
    """
    Given DNA in the aforementioned format,
    return the string "DNA" if the data is DNA,
    return the string "RNA" if the data is RNA,
    return the string "Invalid" if the data is neither DNA nor RNA.
    DNA consists of the following bases:
    Adenine  ('A'),
    Thymine  ('T'),
    Guanine  ('G'),
    Cytosine ('C'),
    RNA consists of the following bases:
    Adenine  ('A'),
    Uracil   ('U'),
    Guanine  ('G'),
    Cytosine ('C'),
    The data is DNA if at least 90% of the bases are one of the DNA bases.
    The data is RNA if at least 90% of the bases are one of the RNA bases.
    The data is invalid if more than 10% of the bases are not one of the DNA or RNA bases.
    Empty bases should be ignored.
    """
    dna_bases = {'A', 'T', 'G', 'C'}
    rna_bases = {'A', 'U', 'G', 'C'}

    total_pairs = len(dna)
    dna_count = 0
    rna_count = 0

    for base1, base2 in dna:
        if base1 and base2:
            if base1 in dna_bases and base2 in dna_bases and base1 in rna_bases and base2 in rna_bases:
                dna_count += 1
                rna_count += 1
            elif base1 in dna_bases and base2 in dna_bases:
                dna_count += 1
            elif base1 in rna_bases and base2 in rna_bases:
                rna_count += 1

    dna_percentage = dna_count / total_pairs
    rna_percentage = rna_count / total_pairs

    if dna_percentage >= 0.9:
        return "DNA"
    elif rna_percentage >= 0.9:
        return "RNA"
    else:
        return "Invalid"


def clean_dna(dna):
    """
    Given DNA in the aforementioned format,
    If the pair is incomplete, ('A', '') or ('', 'G'), ect
    Fill in the missing base with the match base.
    In DNA 'A' matches with 'T', 'G' matches with 'C'
    In RNA 'A' matches with 'U', 'G' matches with 'C'
    If a pair contains an invalid base the pair should be removed.
    Pairs of empty bases should be ignored.
    """
    clean_data = []
    data_type = is_rna(dna)

    for base1, base2 in dna:
        if base1 and base2:
            if base1 == 'A' and base2 == 'T':
                clean_data.append((base1, base2))
            elif base1 == 'T' and base2 == 'A':
                clean_data.append((base1, base2))
            elif base1 == 'G' and base2 == 'C':
                clean_data.append((base1, base2))
            elif base1 == 'C' and base2 == 'G':
                clean_data.append((base1, base2))
            elif base1 == 'A' and base2 == 'U':
                clean_data.append((base1, base2))
            elif base1 == 'U' and base2 == 'A':
                clean_data.append((base1, base2))
        elif base1 and not base2:
            if data_type == 'DNA' and base1 == 'A':
                base2 = 'T'
                clean_data.append((base1, base2))
            elif data_type == 'RNA' and base1 == 'A':
                base2 = 'U'
                clean_data.append((base1, base2))
            elif base1 == 'T':
                base2 = 'A'
                clean_data.append((base1, base2))
            elif base1 == 'G':
                base2 = 'C'
                clean_data.append((base1, base2))
            elif base1 == 'C':
                base2 = 'G'
                clean_data.append((base1, base2))
            elif base1 == 'U':
                base2 = 'A'
                clean_data.append((base1, base2))
        elif base2 and not base1:
            if data_type == 'DNA' and base2 == 'A':
                base1 = 'T'
                clean_data.append((base1, base2))
            elif data_type == 'RNA' and base2 == 'A':
                base1 = 'U'
                clean_data.append((base1, base2))
            elif base2 == 'T':
                base1 = 'A'
                clean_data.append((base1, base2))
            elif base2 == 'G':
                base1 = 'C'
                clean_data.append((base1, base2))
            elif base2 == 'C':
                base1 = 'G'
                clean_data.append((base1, base2))
            elif base2 == 'U':
                base1 = 'A'
                clean_data.append((base1, base2))
                
    return clean_data

def mast_common_base(dna):
    """
    Given DNA in the aforementioned format,
    return the most common first base:
    eg. given:
    A <-> T
    G <-> C
    G <-> C
    C <-> G
    G <-> C
    T <-> A
    The most common first base is 'G'.
    Empty bases should be ignored.
    """
    base_count = {}
    for base1, _ in dna:
        if base1:
            base_count[base1] = base_count.get(base1, 0) + 1

    if not base_count:
        return None

    most_common_base = max(base_count, key=base_count.get)
    return most_common_base

def base_to_name(base):
    """
    Given a base, return the name of the base.
    The base names are:
    Adenine  ('A'),
    Thymine  ('T'),
    Guanine  ('G'),
    Cytosine ('C'),
    Uracil   ('U'),
    return the string "Unknown" if the base isn't one of the above.
    """
    base_names = {
        'A': 'Adenine',
        'T': 'Thymine',
        'G': 'Guanine',
        'C': 'Cytosine',
        'U': 'Uracil',
    }

    return base_names.get(base, 'Unknown')