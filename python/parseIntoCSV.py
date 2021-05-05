qnums = []; 
questions = []; 
ansA = []; 
ansB = []; 
ansC = []; 
ansD = []; 
corAns = []; 

with open("GeneralExamHAM2021.txt") as haminput: 
    for line in haminput: 
        if ("(A)" in line) or  ("(B)" in line) or ("(C)" in line) or ("(D)" in line):
            qname = (line.strip()).split(' ');
            qnums.append(qname[0]);
            corAns.append(qname[1]); 
            next(haminput);
            questions.append(next(haminput).strip());
            next(haminput);
            ansA.append(next(haminput).strip());
            ansB.append(next(haminput).strip());
            ansC.append(next(haminput).strip());
            ansD.append(next(haminput).strip());

print("Q_Number|Question|A|B|C|D|Solution");
for i in range(len(qnums)):
    print(qnums[i] + "|" + questions[i] + "|" + ansA[i] + "|" + ansB[i] + "|" + ansC[i] + "|" + ansD[i] + "|" + corAns[i]);
