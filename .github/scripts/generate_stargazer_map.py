import os
import requests
import pandas as pd
import plotly.express as px
from collections import Counter

GITHUB_TOKEN = os.environ["GITHUB_TOKEN"]
REPO = os.environ["REPO"]

HEADERS = {
    "Authorization": f"Bearer {GITHUB_TOKEN}",
    "Accept": "application/vnd.github+json",
}

def get_all_stargazers():
    """Fetch all stargazers (paginated)."""
    stargazers = []
    page = 1
    while True:
        url = f"https://api.github.com/repos/{REPO}/stargazers?per_page=100&page={page}"
        resp = requests.get(url, headers=HEADERS)
        resp.raise_for_status()
        data = resp.json()
        if not data:
            break
        stargazers.extend(data)
        page += 1
    print(f"Total stargazers: {len(stargazers)}")
    return stargazers

def get_user_location(username):
    """Fetch location field from a user's profile."""
    url = f"https://api.github.com/users/{username}"
    resp = requests.get(url, headers=HEADERS)
    if resp.status_code == 200:
        return resp.json().get("location", None)
    return None

def geocode_location(location, session, cache):
    """Use Nominatim (OSM) to geocode a location string."""
    if not location or location in cache:
        return cache.get(location)
    try:
        url = "https://nominatim.openstreetmap.org/search"
        params = {"q": location, "format": "json", "limit": 1}
        resp = session.get(url, params=params, timeout=5,
                           headers={"User-Agent": f"StargazerMap/{REPO}"})
        results = resp.json()
        if results:
            lat = float(results[0]["lat"])
            lon = float(results[0]["lon"])
            cache[location] = (lat, lon)
            return (lat, lon)
    except Exception:
        pass
    cache[location] = None
    return None

def generate_map(points):
    """Generate an interactive Plotly world map."""
    df = pd.DataFrame(points, columns=["lat", "lon", "location"])
    country_counts = Counter(df["location"])
    df["count"] = df["location"].map(country_counts)

    fig = px.scatter_geo(
        df,
        lat="lat",
        lon="lon",
        hover_name="location",
        size="count",
        size_max=30,
        color="count",
        color_continuous_scale="Plasma",
        projection="natural earth",
        title=f"⭐ Stargazers of {REPO}",
        template="plotly_dark",
    )
    fig.update_layout(
        title_font_size=22,
        geo=dict(
            showframe=False,
            showcoastlines=True,
            coastlinecolor="rgba(255,255,255,0.2)",
            showland=True,
            landcolor="rgb(30, 30, 50)",
            showocean=True,
            oceancolor="rgb(15, 15, 35)",
            showcountries=True,
            countrycolor="rgba(255,255,255,0.1)",
        ),
        paper_bgcolor="rgb(10, 10, 25)",
        font_color="white",
        coloraxis_showscale=False,
        margin=dict(l=0, r=0, t=50, b=0),
    )
    os.makedirs("images", exist_ok=True)
    fig.write_html("images/stargazer-map.html", include_plotlyjs="cdn")
    fig.write_image("images/stargazer-map.svg", width=1200, height=600)
    print("Map saved to images/stargazer-map.html and images/stargazer-map.svg")

def main():
    import time
    stargazers = get_all_stargazers()

    geocache = {}
    points = []

    session = requests.Session()
    for i, user in enumerate(stargazers):
        username = user["login"]
        location = get_user_location(username)
        if location:
            coords = geocode_location(location, session, geocache)
            if coords:
                points.append((coords[0], coords[1], location))
        # Respect Nominatim rate limit: 1 req/sec
        if i % 5 == 0:
            time.sleep(1)

    print(f"Mapped {len(points)} stargazers with locations.")
    if points:
        generate_map(points)
    else:
        print("No geocodable locations found.")

if __name__ == "__main__":
    main()

