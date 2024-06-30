import os
import pandas as pd
import csv
import colorsys

def hex_to_rgb(hex_value):
    hex_value = hex_value.lstrip('#')
    return tuple(int(hex_value[i:i+2], 16) for i in (0, 2, 4))

def rgb_to_cmyk(r, g, b):
    r, g, b = r / 255.0, g / 255.0, b / 255.0
    k = 1 - max(r, g, b)
    if k == 1:
        return 0, 0, 0, 1
    c = (1 - r - k) / (1 - k)
    m = (1 - g - k) / (1 - k)
    y = (1 - b - k) / (1 - k)
    return c, m, y, k

# Get the directory containing the script and CSV files
csv_directory = os.path.dirname(os.path.abspath(__file__))
combined_data = []
index = 1

# Iterate over each CSV file in the directory
for csv_file in os.listdir(csv_directory):
    if csv_file.endswith('.csv') and csv_file != 'all_colornerd_paints.csv':
        csv_path = os.path.join(csv_directory, csv_file)
        
        # Read the CSV file
        try:
            df = pd.read_csv(csv_path, header=None, dtype=str)
            num_columns = df.shape[1]

            # Ignore the header row if it already exists
            if df.iloc[0].str.contains('Name|ID|RGB|Hex', case=False).any():
                print(f"{csv_file} already has headers.")
                continue
            
            # Check if the CSV file has exactly 3 columns
            if num_columns == 3:
                # Trim leading or trailing whitespace from each cell without removing leading zeros
                df = df.applymap(lambda x: x.strip() if isinstance(x, str) else x)
                # Add the header row
                df.columns = ['Color Name', 'ID', 'Hex']
                df['Hex'] = df['Hex'].str.upper()
                # Add new columns
                df['Index'] = range(index, index + len(df))
                df['Source'] = 'ColorNerd'
                df['System'] = os.path.basename(csv_file).replace('.csv', '')

                # Calculate new color columns
                df['Red'] = df['Hex'].apply(lambda x: hex_to_rgb(x)[0])
                df['Green'] = df['Hex'].apply(lambda x: hex_to_rgb(x)[1])
                df['Blue'] = df['Hex'].apply(lambda x: hex_to_rgb(x)[2])

                df['Hue'] = df.apply(lambda row: round(colorsys.rgb_to_hls(row['Red']/255, row['Green']/255, row['Blue']/255)[0]*360, 2), axis=1)
                df['Saturation'] = df.apply(lambda row: round(colorsys.rgb_to_hls(row['Red']/255, row['Green']/255, row['Blue']/255)[2]*100, 2), axis=1)
                df['Luminance'] = df.apply(lambda row: round(colorsys.rgb_to_hls(row['Red']/255, row['Green']/255, row['Blue']/255)[1]*100, 2), axis=1)

                df['Cyan'] = df.apply(lambda row: round(rgb_to_cmyk(row['Red'], row['Green'], row['Blue'])[0]*100, 2), axis=1)
                df['Magenta'] = df.apply(lambda row: round(rgb_to_cmyk(row['Red'], row['Green'], row['Blue'])[1]*100, 2), axis=1)
                df['Yellow'] = df.apply(lambda row: round(rgb_to_cmyk(row['Red'], row['Green'], row['Blue'])[2]*100, 2), axis=1)
                df['Key'] = df.apply(lambda row: round(rgb_to_cmyk(row['Red'], row['Green'], row['Blue'])[3]*100, 2), axis=1)
                
                index += len(df)
                
                # Rearrange columns
                df = df[['Index', 'Color Name', 'ID', 'Hex', 'Red', 'Green', 'Blue', 'Hue', 'Saturation', 'Luminance', 'Cyan', 'Magenta', 'Yellow', 'Key', 'Source', 'System']]
                
                # Append to combined data
                combined_data.append(df)
                
                # Save the updated individual CSV file
                df.to_csv(csv_path, index=False, quoting=csv.QUOTE_ALL)
                print(f"Processed {csv_file}")
            else:
                # Print the name of the CSV file
                print(f"{csv_file} does not have 3 columns. It has {num_columns} columns.")
        
        except Exception as e:
            print(f"Error processing {csv_file}: {e}")

# Combine all data into a single DataFrame
combined_df = pd.concat(combined_data, ignore_index=True)

# Save the combined DataFrame to a single CSV file
combined_csv_path = os.path.join(csv_directory, 'all_colornerd_paints.csv')
combined_df.to_csv(combined_csv_path, index=False, quoting=csv.QUOTE_ALL)

print(f"Combined data saved to {combined_csv_path}")
