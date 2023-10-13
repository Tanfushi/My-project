import requests
import json
import pandas as pd
import matplotlib.pyplot as plt
import geopandas as gpd

# ==========================
# Step 1: Data Collection
# ==========================
def fetch_food_truck_data(api_key, location, radius=1500, type_="food_truck"):
    url = f"https://maps.googleapis.com/maps/api/place/nearbysearch/json?location={location}&radius={radius}&type={type_}&key={api_key}"
    response = requests.get(url)
    data = json.loads(response.text)
    return data['results']

# ==========================
# Step 2: Data Cleaning
# ==========================
def clean_data(raw_data):
    food_trucks = []
    for place in raw_data:
        name = place.get('name')
        address = place.get('vicinity')
        rating = place.get('rating', 'N/A')
        website = place.get('website', 'N/A')
        open_hours = place.get('opening_hours', {}).get('weekday_text', 'N/A')
        cuisine_type = place.get('types', ['N/A'])[0]
        food_trucks.append([name, address, rating, website, open_hours, cuisine_type])
        
    df = pd.DataFrame(food_trucks, columns=['Name', 'Address', 'Rating', 'Website', 'Open Hours', 'Cuisine Type'])
    return df

# ==========================
# Step 3: Data Analysis
# ==========================
def basic_analysis(df):
    avg_rating = df[df['Rating'] != 'N/A']['Rating'].astype(float).mean()
    most_common_cuisine = df['Cuisine Type'].value_counts().idxmax()
    return avg_rating, most_common_cuisine

# ==========================
# Step 4: Data Visualization
# ==========================
def plot_ratings(df):
    df[df['Rating'] != 'N/A'].astype({'Rating': 'float'}).plot(kind='bar', x='Name', y='Rating', legend=False)
    plt.title('Food Truck Ratings')
    plt.xlabel('Food Truck')
    plt.ylabel('Rating')
    plt.tight_layout()
    plt.savefig('visualizations/ratings_plot.png')

# ==========================
# Step 5: Weekend Foodie Plan (Manual)
# ==========================
def generate_foodie_plan(df):
    # Manually select based on rating and variety, and save as CSV (Example only)
    plan = df[df['Rating'] != 'N/A'].nlargest(5, 'Rating')
    plan['Day'] = ['Saturday', 'Saturday', 'Sunday', 'Sunday', 'Sunday']
    plan['Time'] = ['12:00 PM', '3:00 PM', '10:00 AM', '1:00 PM', '4:00 PM']
    plan.to_csv('analysis/foodie_plan.csv', index=False)

# ==========================
# Step 6: Route Visualization (Example)
# ==========================
def plot_route():
    # Generate an example GeoDataFrame
    gdf = gpd.GeoDataFrame(
        {'geometry': [Point(0, 0), Point(1, 1), Point(2, 2), Point(3, 3)],
         'Name': ['Truck A', 'Truck B', 'Truck C', 'Truck D']})
    
    # Plotting
    gdf.plot()
    plt.title('Food Truck Route')
    plt.xlabel('Longitude')
    plt.ylabel('Latitude')
    plt.savefig('visualizations/route_map.png')

# ==========================
# Main Function
# ==========================
if __name__ == "__main__":
    API_KEY = "YOUR_API_KEY"
    LOCATION = "YOUR_LOCATION"
    
    # Fetch and Clean Data
    raw_data = fetch_food_truck_data(API_KEY, LOCATION)
    df = clean_data(raw_data)
    df.to_csv('data/food_truck_data.csv', index=False)
    
    # Basic Analysis
    avg_rating, most_common_cuisine = basic_analysis(df)
    print(f"Average Rating: {avg_rating}, Most Common Cuisine: {most_common_cuisine}")
    
    # Data Visualization
    plot_ratings(df)
    
    # Generate Foodie Plan
    generate_foodie_plan(df)
    
    # Route Visualization (Example)
    plot_route()

