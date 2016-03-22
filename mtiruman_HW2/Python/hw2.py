import re

def hw2(inFile, outFile):
	inputFile = open(inFile, "r")
	outputFile = open(outFile, "w")
	stack = []
	for line in inputFile:
		if ":error:" in line:
			stack.append(":error:")
		elif ":true:" in line:
			stack.append(":true:")
		elif ":false:" in line:
			stack.append(":false:")
		elif "neg" in line:
			if len(stack) == 0:
				stack.append(":error:")
			elif len(stack) > 0:
				if isinstance(stack[-1], str):
					stack.append(":error:")
				elif isinstance(stack[-1], int):
					a = (-1)*(stack.pop())
					stack.append(a)
		elif "push" in line:
			num = re.findall("[-+]?\d+[\.]?\d*",line)
			if "." in num[0]:
				stack.append(":error:")
			else:
				stack.append(int(num[0]))
		elif "pop" in line:
			if len(stack) == 0:
				stack.append(":error:")
			else:
				stack.pop()
		elif "add" in line:
			if len(stack) <= 1:
				stack.append(":error:")
			else:
				if (isinstance(stack[-1], int) and isinstance(stack[-2], int)):
					y = int(stack.pop())
					x = int(stack.pop())
					stack.append(y+x)
				else:
					stack.append(":error:")
		elif "sub" in line:
			if len(stack) <= 1:
				stack.append(":error:")
			else:
				if (isinstance(stack[-1], int) and isinstance(stack[-2], int)):
					y = int(stack.pop())
					x = int(stack.pop())
					stack.append(x-y)
				else:
					stack.append(":error:")
		elif "mul" in line:
			if len(stack) <= 1:
				stack.append(":error:")
			else:
				if (isinstance(stack[-1], int) and isinstance(stack[-2], int)):
					y = int(stack.pop())
					x = int(stack.pop())
					stack.append(y*x)
				else:
					stack.append(":error:")
		elif "div" in line:
			if len(stack) <= 1:
				stack.append(":error:")
			else:
				if (isinstance(stack[-1], int) and isinstance(stack[-2], int)):
					y = int(stack.pop())
					x = int(stack.pop())
					if y == 0:
						stack.append(x)
						stack.append(y)
						stack.append(":error:")
					else:
						stack.append(x/y)
				else:
					stack.append(":error:")
		elif "rem" in line:
			if len(stack) <= 1:
				stack.append(":error:")
			else:
				if (isinstance(stack[-1], int) and isinstance(stack[-2], int)):
					y = int(stack.pop())
					x = int(stack.pop())
					if y == 0:
						stack.append(x)
						stack.append(y)
						stack.append(":error:")
					else:
						stack.append(x%y)
				else:
					stack.append(":error:")
		elif "swap" in line:
			if len(stack) <= 1:
				stack.append(":error:")
			else:
				temp = stack[-1]
				stack[-1] = stack[-2]
				stack[-2] = temp
		elif "quit" in line:
			break
	stack.reverse()
	for i in stack:
		outputFile.write(str(i) + "\n")