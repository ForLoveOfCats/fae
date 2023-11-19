import sys
import random

SEED = 123
INDENT = "    "

FILE_COUNT = 1
ITEM_COUNT = 2
EXPRESSION_COUNT = 2
MAX_EXPRESSION_DEPTH = 5
MAX_INNER_FUNCTION_DEPTH = 3

def write(format):
	sys.stdout.write(format)

def writeln(format):
	sys.stdout.write(format)
	sys.stdout.write("\n")

class Catagory:
	def __init__(self, parent):
		self.parent = parent
		self.parent_len = 0
		if self.parent != None:
			self.parent_len = self.parent.len()

		self.items = []

	def len(self):
		return self.parent_len + len(self.items)

	def append(self, item):
		self.items.append(item)

	def get(self, index):
		if index >= self.parent_len:
			return self.items[index - self.parent_len]
		else:
			self.parent.get(index)

	def pick(self):
		index = random.randrange(self.len())
		return self.get(index)

class Scope:
	def __init__(self, parent, function_depth):
		self.function_depth = function_depth
		self.depth = parent.depth + 1 if parent != None else 0
		self.all = Catagory(parent.all if parent != None else None)

		self.catagorized = {}
		for kind in [
			FaeVoid,
			FaeU8,
			FaeU16,
			FaeU32,
			FaeU64,
			FaeUsize,
			FaeI8,
			FaeI16,
			FaeI32,
			FaeI64,
			FaeF32,
			FaeF64,
			FaeInteger,
			FaeDecimal,
			FaeStruct
		]:
			parent_catagory = parent.catagorized[kind] if parent != None else None;
			self.catagorized[kind] = Catagory(parent_catagory)

	def child(self):
		return Scope(self, self.function_depth)

	def child_for_function(self):
		return Scope(self, self.function_depth + 1)

	def append(self, item):
		self.all.append(item)
		self.catagorized[item.fae_type].append(item)

class FaeVoid:
	pass

class FaeU8:
	pass
class FaeU16:
	pass
class FaeU32:
	pass
class FaeU64:
	pass
class FaeUsize:
	pass

class FaeI8:
	pass
class FaeI16:
	pass
class FaeI32:
	pass
class FaeI64:
	pass

class FaeF32:
	pass
class FaeF64:
	pass

class FaeInteger:
	pass
class FaeDecimal:
	pass

class FaeStruct:
	pass

class Constant:
	def __init__(self, id, name, fae_type):
		self.id = id
		self.name = name
		self.fae_type = fae_type

class Function:
	def __init__(self, id, name, fae_type):
		self.id = id
		self.name = name
		self.fae_type = fae_type

class File:
	def __init__(self, items):
		self.items = items

files = []

def next_id():
	id = next_id.next_id
	next_id.next_id += 1
	return id
next_id.next_id = 0

def generate_indentation(scope):
	for _ in range(scope.depth):
		sys.stdout.write(INDENT)

def generate_constant(scope):
	id = next_id()
	name = f"constant_{id}";
	constant = Constant(id, name, FaeVoid)
	scope.append(constant)

	generate_indentation(scope);
	writeln(f"const {name} = ")

def generate_function(scope):
	id = next_id()
	name = f"function_{id}";
	function = Function(id, name, FaeVoid)
	scope.append(function)
	
	scope = scope.child_for_function()
	
	generate_indentation(scope);
	writeln(f"fn {name} {{")

	for _ in range(EXPRESSION_COUNT):
		choices = [generate_constant]
		if scope.function_depth <= MAX_INNER_FUNCTION_DEPTH:
			choices.append(generate_function)

		random.choice(choices)(scope)

def generate_file():
	scope = Scope(None, 0)

	for _ in range(ITEM_COUNT):
		random.choice([
			generate_constant,
			generate_function,
		])(scope)

	files.append(File(scope.all.items))

def main():
	random.seed(SEED)

	for _ in range(FILE_COUNT):
		generate_file()

if __name__ == "__main__":
	main()
