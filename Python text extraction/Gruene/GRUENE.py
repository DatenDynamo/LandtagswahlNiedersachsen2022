import pdfplumber

def get_filtered_text(file_to_parse: str) -> str:
    with pdfplumber.open(file_to_parse) as pdf: 
        page = pdf.pages[20]
        left = page.crop((0, 0.069 * page.height, 0.51486 * page.width, 0.93 * page.height))
        right = page.crop((0.51486 * page.width, 0.069 * page.height, page.width, 0.93* page.height))
        l_text = left.extract_text()
        r_text = right.extract_text()
        text = l_text + " " + r_text
        clean_text = text.filter(lambda obj: not (obj["object_type"] == "char" and ("PTSans-BoldItalic" in obj["fontname"] and obj["size"] != 11) or ("PTSans-Regular" in obj["fontname"] and obj["size"] != 10) ))
        print(clean_text.extract_text())
get_filtered_text("./gruene.pdf") 