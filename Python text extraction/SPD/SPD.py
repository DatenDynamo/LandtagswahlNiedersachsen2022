import pdfplumber

crop_coords = [0, 0.168, 1, 0.93]

def get_filtered_text(file_to_parse: str) -> str:
    with pdfplumber.open(file_to_parse) as pdf:  
        for page in pdf.pages:
            my_width = page.width
            my_height = page.height
            my_bbox = (crop_coords[0]*float(my_width), crop_coords[1]*float(my_height), crop_coords[2]*float(my_width), crop_coords[3]*float(my_height))
            page_crop = page.crop(bbox=my_bbox)
            text = str(page_crop.extract_text()).lower()
            clean_text = page_crop.filter(lambda obj: not (obj["object_type"] == "char" and (("Karla ExtraBold" in obj["fontname"] and obj["size"] != 10) or ("Archivo Narrow" in obj["fontname"] and obj["size"] != 10))))
            print(clean_text.extract_text())

get_filtered_text("./SPD.pdf")
