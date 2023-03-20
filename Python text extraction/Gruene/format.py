import re

with open('text_korrekt.txt', 'r', encoding = "utf-8") as f:
    lines = f.readlines()

formatted_text = ""
for line in lines:
    if line.startswith("#"):
        formatted_text += "\n" + line.strip() + "\n"
    else:
        # Satzerkennunng mit Hilfe von RegEx
        sentences = re.split(r'(?<=[.!?])\s+', line.strip())
        for i, sentence in enumerate(sentences):
            # Prüfe ob Satz ein Satz ist durch erkennen von Groß- und Kleinschreibung und Interpunktion
            # oder ob es sich um Abk. handelt ("z.B.") oder ... . Ignoriere wenn die Abk. innerhalb des Satzes steht 
            if re.match(r'^[A-ZÄÖÜ].*?[.!?]\s*$|^[a-zäöüß][.][a-zäöüß][.]($|\s)|^.*?\.{3}\s*$', sentence) or not re.search(r'\b[a-zäöüß][.][a-zäöüß][.]\b', sentence):
                formatted_text += sentence.strip()
                if i < len(sentences) - 1:
                    formatted_text += "\n"
                else:
                    formatted_text += " "

print(formatted_text)
