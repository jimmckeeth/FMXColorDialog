import os
import requests
from bs4 import BeautifulSoup
import pandas as pd

# Base URL of the website
base_url = "https://www.color-hex.com"

# Function to convert RGB to HEX
def rgb_to_hex(color):
    if color.startswith('#'):
        return color
    rgb = color.replace('rgb(', '').replace(')', '').split(',')
    return "#{:02x}{:02x}{:02x}".format(int(rgb[0]), int(rgb[1]), int(rgb[2]))

# Function to scrape palette data from a given URL
def scrape_palette_data(url, category):
    response = requests.get(url)
    soup = BeautifulSoup(response.content, 'html.parser')
    
    palette_elements = soup.find_all('div', class_='col-xs-6 col-md-3')
    data = []

    for palette in palette_elements:
        title_element = palette.find('a', title=True)
        title = title_element['title'].replace('Color palette ', '')
        link = title_element['href']

        color_elements = palette.select('.palettecolordivcon .palettecolordiv')
        color_values = [rgb_to_hex(color['style'].split(':')[-1].strip()) for color in color_elements[:5]]

        # Ensure there are exactly 5 colors
        while len(color_values) < 5:
            color_values.append('')

        data.append([category] + color_values + [title, base_url + link])
    
    return data

# Function to scrape all categories and their respective palettes
def scrape_all_palettes(main_url):
    response = requests.get(main_url)
    soup = BeautifulSoup(response.content, 'html.parser')
    
    tag_list_div = soup.find('div', class_='taglistDiv')
    if not tag_list_div:
        print("Error: 'taglistDiv' not found on the main page.")
        return []
    
    category_links = tag_list_div.find_all('a')

    all_data = []

    for link in category_links:
        category_url = base_url + link['href']
        category = link['title']
        print(f"Scraping category: {category}")
        category_data = scrape_palette_data(category_url, category)
        all_data.extend(category_data)

    return all_data

# Main URL of the website
main_url = "https://www.color-hex.com/color-palettes/"

# Scrape all palette data
all_palette_data = scrape_all_palettes(main_url)

if all_palette_data:
    # Create a DataFrame and save to CSV in the same folder as the script
    df = pd.DataFrame(all_palette_data, columns=['category', 'color1', 'color2', 'color3', 'color4', 'color5', 'title', 'link'])
    script_dir = os.path.dirname(os.path.abspath(__file__))
    csv_filename = os.path.join(script_dir, 'color_palettes.csv')
    df.to_csv(csv_filename, index=False)
    print(f"Data successfully saved to {csv_filename}")
else:
    print("No data to save.")
