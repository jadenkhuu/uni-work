#!/usr/bin/env python3
import sys
import re

# Accumulated lines to print
linesToPrint = []

# Lines to print at the top of the file
importModules = []

# Array of variable names which are glob related
globbed = []

# external_commands = ["touch", "ls", "mkdir", "chmod", "rm", "pwd"]

def comment_inline_func(line):
    res = ""
    if '#' in line: 
        l = line.split('#', 1)
        res = " #" + re.sub(r'\n', "", l[1])
    return res

def glob_func(res, glob): # helper function to append glob string to resulting string
    if res == "\"":
        return glob
    elif re.search(r'\"$', res):
        return res.rstrip('"').rstrip(' +') + " + " + glob
    else:
        return res + "\" + " + glob

def echo_func(line, indent):
    comment = comment_inline_func(line)
    l = line.split("#", 1)[0].split()
    
    # print(line.split("#", 1)[0].split("echo")[1])
    
    if len(l) == 1:
        linesToPrint.append("print()")
        return
    
    if re.search(r'^\'', l[1]) and re.search(r'\'$', l[-1]):
        string = line.split("#", 1)[0].rstrip("\n").lstrip("echo ")
        res = f"print({string})" + comment
        linesToPrint.append(res)
        return
    
    res = "\"" 

    isGlob = False
    for var in l[1:]: # for each word in the echoed line
        if var == "":
            continue
        
        elif var in ["*", "?", "[", "]"]: # singular globbed char
            res = "print(\" \".join(sorted(glob.glob(\"" + var + "\"))))"
            importModules.append("import glob")
            linesToPrint.append(res)
            return
                
        elif re.search(r'\$\{[a-zA-Z0-9]+\}', var):
            var = re.sub(r'\$', "", var) + " "
            res = res + var
            
        elif re.search(r'^\$', var):
            if re.search(r'\$[0-9]', var): # Command line arguments
                importModules.append("import sys")
                res = res + "{sys.argv[" + var[1:] + "]}"
            
            elif var[1:] in globbed: # If variable is a glob related - in globbed list
                importModules.append("import glob")
                isGlob = True     
                glob = "\" \".join(sorted(glob.glob(" + var[1:] + ")))"  
                res = glob_func(res, glob) + " + " + "\""
            else: 
                # remove $ from variable and substitute with {}
                word = re.sub(r'\$([a-zA-Z_]\w*)', lambda match: f"{{{match.group(1)}}}", var.rstrip()) + " "
                res = res + word
                
        elif re.search(r'[\[\*\?\]]+', var): # inline globbed variable name
            importModules.append("import glob")
            isGlob = True
            glob = "\" \".join(sorted(glob.glob(\"" + var + "\")))"
            res = glob_func(res, glob) + " + " + "\""
            continue  
        
        else:
            res = res + var + " "

    if isGlob:
        res = res.rstrip() + "\""
        # determines if there are any extra " or + symbols and removes them
        if re.search(r'\"\"$', res):
            res = res.rstrip('"').rstrip(" +")
    else: 
        res = res.strip() + "\""
        
    printString = "print(f" + res + ")" + comment
    linesToPrint.append((indent  * "    ") + printString)
    return

def variable_func(line, indent):
    comment = comment_inline_func(line)
    l = line.split("#", 1)[0].split("=")

    varName = l[0]
    if varName == "print":
        linesToPrint.append("Variable name is \"print\"")
        return
    if varName == "pass":
        linesToPrint.append("Variable name is \"pass\"")
        return

    if re.search(r'[\[*?\]]+', l[1]):
        importModules.append("import glob")
        globbed.append(varName)
        
        printString = varName + " = " + "\"" + l[1].strip() + "\"" + comment
        
    elif re.search(r'\$[0-9]', l[1]): # Command line arguments
        importModules.append("import sys")
        
        var = re.sub(r'\$', "", l[1])        
        printString = varName + " = sys.argv[" + var.strip() + "]"
    
    elif re.search(r'\$', l[1]):
        vars = l[1].split("$")
        res = ""
        
        for var in vars:
            if var == "":
                continue
            res = res + "{" + var.rstrip() + "}"

        res = "f\"" + res + "\""
        printString = varName + " = " + res + comment
        
    else:
        printString = varName + " = " + "\"" + l[1].strip() + "\"" + comment

    linesToPrint.append((indent  * "    ") + printString)

def exit_func(line, indent):
    comment = comment_inline_func(line)
    l = line.split("#", 1)[0].split("=")
    res = "sys.exit("
    
    if len(l[0].split()) > 1:
        exitCode = line.split()[1]
        res = res + exitCode + ")" + comment
    else:
        res = res + ")" + comment
    
    importModules.append("import sys")
    linesToPrint.append((indent  * "    ") + res)
    
def for_func(line, indent):
    comment = comment_inline_func(line)
    l = line.split("#", 1)[0].split("in", 1)

    # the list consists of a globbing element
    forList = l[1].split()
    for elem in forList:
        if re.search(r'[\[*?\]]+', l[1]):
            importModules.append("import glob")
            forList = f"sorted(glob.glob(\"{elem}\"))"
        
    printString = (indent * "    ") + f"{l[0]}in {forList}:{comment}"
    linesToPrint.append(printString)
    
def cd_func(line, indent):
    comment = comment_inline_func(line)
    l = line.split("#", 1)[0].split()
    importModules.append("import os")
    
    echo = (indent * '    ') + "os.chdir(\"" + l[1] + "\")" + comment
    linesToPrint.append(echo)
    
def read_func(line, indent):
    comment = comment_inline_func(line)
    l = line.split("#", 1)[0].split()
    
    res = (indent * '    ') + l[1] + " = " "input()" + comment
    linesToPrint.append(res)
    
def external_func(line, indent):
    comment = comment_inline_func(line)
    l = line.split("#", 1)[0].split()
    importModules.append("import subprocess")
    res = "subprocess.run(["
    
    for var in l:
        if re.search(r'\$', var):
            var = re.sub(r'^\$', "", var)
            res = res + var + ", "
        else:
            res = res + "\"" + var + "\", "

    res = res.rstrip(" ").rstrip(",")
    printString = (indent * '    ') + res + "])" + comment
    linesToPrint.append(printString)
    
def test_var_helper(res, var):
    if res == "\"":
        return var
    elif re.search(r'\$', var):
        
        return

def if_test_func(line, indent):
    importModules.append("import os")
    
    comment = comment_inline_func(line)
    l = line.split("#", 1)[0].split()
    # l = ["if", "test", "" ...]
    res = ""
    
    # File operations
    if "-a" in l[2:]:
        res = f"{l[2]} and {l[4]}"
    elif "-o" in l[2:]:
        res = f"{l[2]} or {l[4]})"
    elif "=" in l[2:]:
        res = f"\'{l[2]}\' == \'{l[4]}\'"
    elif "!=" in l[2:]:
        res = f"\'{l[2]}\' != \'{l[4]}\'"
    elif "-eq" in l[2:]:
        res = f"\'{l[2]}\' == \'{l[4]}\'"
    elif "-ge" in l[2:]:
        res = f"\'{l[2]}\' >= \'{l[4]}\'"
    elif "-gt" in l[2:]:
        res = f"\'{l[2]}\' > \'{l[4]}\'"
    elif "-le" in l[2:]:
        res = f"\'{l[2]}\' <= \'{l[4]}\'"
    elif "-lt" in l[2:]:
        res = f"\'{l[2]}\' < \'{l[4]}\'"
    elif "-ne" in l[2:]:
        res = f"\'{l[2]}\' != \'{l[4]}\'"
    elif "-ef" in l[2:]:
        res = f"os.path.samefile({l[2]}, {l[4]})"
    elif "-nt" in l[2:]:
        res = f"os.path.getmtime({l[2]}) > os.path.getmtime({l[4]})"
    elif "-ot" in l[2:]:
        res = f"os.path.getmtime({l[2]}) < os.path.getmtime({l[4]})"   

    if "-w" in l[2:]:
        res = f"os.access(\"{l[3]}\", os.W_OK)"
    elif "-e" in l[2:]:
        res = f"os.path.isfile(\"{l[3]}\")"        
    elif "-d" in l[2:]:
        res = f"os.path.isdir(\"{l[3]}\")"
    elif "-G" in l[2:]:
        res = f"(os.path.exists(\"{l[3]}\") and stat.S_ISGID(os.stat(\"{l[3]}\").st_mode))"
    elif "-c" in l[2:]:
        res = f"(os.path.exists(\"{l[3]}\") and stat.S_ISCHR(os.stat(\"{l[3]}\").st_mode))"
    elif "-r" in l[2:]:
        res = f"os.access(\"{l[3]}\", os.R_OK)"
    elif "-f" in l[2:]:
        res = f"(os.path.exists(\"{l[3]}\") and stat.S_ISREG(os.stat(\"{l[3]}\").st_mode))"
    elif "-g" in l[2:]:
        res = f"(os.path.exists(\"{l[3]}\") and stat.S_IMODE(os.stat(\"{l[3]}\").st_mode))"
    elif "-L" in l[2:]:
        res = f"(os.path.exists(\"{l[3]}\") and stat.S_ISLNK(os.stat(\"{l[3]}\").st_mode))"
    elif "-b" in l[2:]:
        res = f"(os.path.exists(\"{l[3]}\") and stat.S_ISBLK(os.stat(\"{l[3]}\").st_mode)))"
    elif "-h" in l[2:]:
        res = f"(os.path.exists(\"{l[3]}\") and stat.S_ISLNK(os.stat(\"{l[3]}\").st_mode))"
    elif "-k" in l[2:]:
        res = f"(os.path.exists(\"{l[3]}\") and stat.S_ISVTX(os.stat(\"{line[3]}\").st_mode))"
    
    printString = (indent * '    ') + f"if {res}: {comment}"    
    linesToPrint.append(printString)
    return

def elif_test_func(line, indent):
    importModules.append("import os")
    
    comment = comment_inline_func(line)
    l = line.split("#", 1)[0].split()
    # l = ["if", "test", "" ...]
    res = ""
    
    # File operations
    if "-a" in l[2:]:
        res = f"{l[2]} and {l[4]}"
    elif "-o" in l[2:]:
        res = f"{l[2]} or {l[4]})"
    elif "=" in l[2:]:
        res = f"\'{l[2]}\' == \'{l[4]}\'"
    elif "!=" in l[2:]:
        res = f"\'{l[2]}\' != \'{l[4]}\'"
    elif "-eq" in l[2:]:
        res = f"\'{l[2]}\' == \'{l[4]}\'"
    elif "-ge" in l[2:]:
        res = f"\'{l[2]}\' >= \'{l[4]}\'"
    elif "-gt" in l[2:]:
        res = f"\'{l[2]}\' > \'{l[4]}\'"
    elif "-le" in l[2:]:
        res = f"\'{l[2]}\' <= \'{l[4]}\'"
    elif "-lt" in l[2:]:
        res = f"\'{l[2]}\' < \'{l[4]}\'"
    elif "-ne" in l[2:]:
        res = f"\'{l[2]}\' != \'{l[4]}\'"
    elif "-ef" in l[2:]:
        res = f"os.path.samefile({l[2]}, {l[4]})"
    elif "-nt" in l[2:]:
        res = f"os.path.getmtime({l[2]}) > os.path.getmtime({l[4]})"
    elif "-ot" in l[2:]:
        res = f"os.path.getmtime({l[2]}) < os.path.getmtime({l[4]})"   

    if "-w" in l[2:]:
        res = f"os.access(\"{l[3]}\", os.W_OK)"
    elif "-e" in l[2:]:
        res = f"os.path.isfile(\"{l[3]}\")"        
    elif "-d" in l[2:]:
        res = f"os.path.isdir(\"{l[3]}\")"
    elif "-G" in l[2:]:
        res = f"(os.path.exists(\"{l[3]}\") and stat.S_ISGID(os.stat(\"{l[3]}\").st_mode))"
    elif "-c" in l[2:]:
        res = f"(os.path.exists(\"{l[3]}\") and stat.S_ISCHR(os.stat(\"{l[3]}\").st_mode))"
    elif "-r" in l[2:]:
        res = f"os.access(\"{l[3]}\", os.R_OK)"
    elif "-f" in l[2:]:
        res = f"(os.path.exists(\"{l[3]}\") and stat.S_ISREG(os.stat(\"{l[3]}\").st_mode))"
    elif "-g" in l[2:]:
        res = f"(os.path.exists(\"{l[3]}\") and stat.S_IMODE(os.stat(\"{l[3]}\").st_mode))"
    elif "-L" in l[2:]:
        res = f"(os.path.exists(\"{l[3]}\") and stat.S_ISLNK(os.stat(\"{l[3]}\").st_mode))"
    elif "-b" in l[2:]:
        res = f"(os.path.exists(\"{l[3]}\") and stat.S_ISBLK(os.stat(\"{l[3]}\").st_mode)))"
    elif "-h" in l[2:]:
        res = f"(os.path.exists(\"{l[3]}\") and stat.S_ISLNK(os.stat(\"{l[3]}\").st_mode))"
    elif "-k" in l[2:]:
        res = f"(os.path.exists(\"{l[3]}\") and stat.S_ISVTX(os.stat(\"{line[3]}\").st_mode))"
    
    printString = (indent * '    ') + f"elif {res}: {comment}"    
    linesToPrint.append(printString)
    return

############################################################

# (r'$([a-zA-Z_]\w*)', var)
# regex to determine variables $x

def main():
    f = open(sys.argv[1], "r")
    lines = f.readlines()
    f.close()
        
    # Determine if operations are nested
    
    process_lines(lines)

    print("#!/usr/bin/python3 -u\n")
    for i in list(set(importModules)):
        print(i)

    for i in linesToPrint:
        print(i)

    return

def process_lines(lines):
    indent = 0
    for line in lines:
        if line == "\n":
            linesToPrint.append("")
            continue

        # remove indents from each line
        line = line.lstrip()
        
        if (re.search(r'^#', line)) and ("#!/bin/dash" not in line): # Comment only
            linesToPrint.append(line.rstrip())
            continue
        elif "#!/bin/dash" in line:
            continue
        elif re.search(r'^echo', line):
            echo_func(line, indent)
            continue
        elif re.search(r'^then', line):
            indent += 1
            continue
        elif re.search(r'^elif', line):
            indent -= 1
            elif_test_func(line, indent)
            continue
        elif re.search(r'^if', line):
            if_test_func(line, indent)
            continue
        elif re.search(r'^else', line):
            linesToPrint.append("else:")
            continue
        elif re.search(r'^fi', line):
            indent -= 1
            continue
        elif re.search(r'=', line):
            variable_func(line, indent)
            continue
        elif re.search(r'^exit', line):
            exit_func(line, indent)            
            continue
        elif re.search(r'^for', line):
            for_func(line, indent)
            continue
        elif re.search(r'^done', line):
            indent -= 1
            continue
        elif re.search(r'^do', line):
            indent += 1
            continue
        elif re.search(r'^cd', line):
            cd_func(line, indent)
            continue
        elif re.search(r'^read', line):
            read_func(line, indent)
            continue
        else:
            external_func(line, indent)
            continue
        
if __name__ == "__main__":
    main()