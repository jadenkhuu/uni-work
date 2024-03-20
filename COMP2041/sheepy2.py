#!/usr/bin/env python3
import sys
import re

def echo_func(line):
    commentExists = False
    if "#" in line:
        comment = "#" + line.split("#")[1]
        line = line.split("#")[0]
        commentExists = True
    # elif re.search(r'[\*\?\[\]]'):
        
    l = line.split()
    echo = ""
    
    for word in l[1:]:
        if re.search(r'^\$', word):
            variable = word.split('$')
            echo = echo + "{" + variable[1] + "}" + " "
        else:
            echo = echo + word + " "
    
    echo = echo.strip()
    if commentExists == False:
        res = "print(f\"" + echo + "\")"
        print(f"{res.strip()}")
    else:
        res = "print(f\"" + echo + "\") " + comment
        print(f"{res.strip()}")

def variable_func(line):
    commentExists = False
    if "#" in line:
        comment = "#" + line.split("#")[1]
        line = line.split("#")[0]
        commentExists = True
    
    l = line.split("=")
    res = ""
    
    if "$" in l[1]:
        var = l[1].split("$")
        
        for char in var:
            res = res + "{" + char.rstrip() + "}"

        res = res.strip()
        if commentExists == False:
            print(f"{l[0]} = f\"{res}\"")
        else:
            print(f"{l[0]} = f\"{res}\" {comment}")
    else:    
        if commentExists == False:
            res = l[1].strip()
            print(f"{l[0]} = \"{res}\"")
        else:
            print(f"{l[0]} = \"{res}\" {comment}")
                       
def comment_inline(line):
    l = line.split("#")
    
    if len(l) > 1: # comment in a line
        print(f"exists {line}")
        
    commentExists = False
    if "#" in line:
        comment = "#" + line.split("#")[1]
        line = line.split("#")[0]
        commentExists = True

def forloop_func(line):
    
    return

def read_func(line):
    
    return

def main():
    f = open(sys.argv[1], "r")
    lines = f.readlines()
    f.close()

    for line in lines:
        if "#!/bin/dash" in line:
            print("#!/usr/bin/python3 -u")
            continue

        if "\n" == line:
            print()
            continue
                
        if re.search(r'^#', line):
            print(line, end="")
            continue
        
        if re.search(r'^exit', line):
            print(f"sys.exit({line.split[1]})")
            continue
       
        if re.search(r'^echo', line):
            echo_func(line)
            continue
        
        if re.search(r'^for', line):
            forloop_func(line)
            continue
        
        if re.search(r'^read', line):
            read_func(line)
            continue
        
        if re.search(r'.*=.+', line):
            variable_func(line)
            continue

if __name__ == "__main__":
    main()


# for line in lines:
#     if line == "\n":
#         temppy.write("\n")
    
#     if "echo" in line:
#         temppy.write(f'print("{" ".join(line.split()[1:])}")\n')
    
#     if "=" in line:
#         i = line.find("=")
#         temppy.write(f"{line[:i]} = \"{line[i+1:]}\"\n")
        