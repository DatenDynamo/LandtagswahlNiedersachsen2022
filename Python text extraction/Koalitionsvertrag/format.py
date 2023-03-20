import re

with open('Koalitionsvertrag_utf8.txt', 'r', encoding ='utf-8') as f:
    lines = f.readlines()

formatted_text = ""
for line in lines:
    if line.startswith("#"):
        formatted_text += "\n" + line.strip() + "\n"
    else:
        # Split text into sentences using a regular expression
        sentences = re.split(r'(?<=[.!?])\s+', line.strip())
        for sentence in sentences:
            # Check if the sentence is a valid sentence by checking if it starts with a capital letter
            if re.match(r'^[A-ZÄÖÜ].*?[.!?]\s*$', sentence):
                formatted_text += sentence.strip() + "\n"

print(formatted_text)
