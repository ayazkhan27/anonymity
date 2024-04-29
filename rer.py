# Defining the costs and impacts for media types
media_costs = [500000, 300000, 150000, 50000]
media_impacts = [1500000, 600000, 800000, 400000]

# Calculating total costs and total impacts
total_media_cost = sum(media_costs)
total_media_impact = sum(media_impacts)

# Calculating ROI
media_roi = ((total_media_impact - total_media_cost) / total_media_cost) * 100
media_roi
