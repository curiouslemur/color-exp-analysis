#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Apr  8 22:16:24 2024

@author: fkhmaissia
"""

import pandas as pd
import numpy as np

# Load the data
df = pd.read_csv('mg-weight.csv')

# Pivot the dataframe to create the matrix M
M = df.pivot(index='color', columns='concept', values='weight')

# def get_top_k_colors_(C1, C2, K):
#     # Compute D1 and D2
#     D1 = (M[C1] - M[C2])/(M[C2]**2)
#     D2 = (M[C2] - M[C1])/(M[C1]**2)

#     # Sort D1 and D2 in descending order
#     D1_sorted = D1.sort_values(ascending=False)
#     D2_sorted = D2.sort_values(ascending=False)

#     # Get the top K colors
#     top_k_D1_colors = D1_sorted.index[:K].tolist()
#     top_k_D2_colors = D2_sorted.index[:K].tolist()

#     return top_k_D1_colors, top_k_D2_colors

def get_top_k_colors(C1, C2, K):
    # Compute D1 and D2
    D1 = (M[C1] - M[C2])/(M[C2]**2)
    D2 = (M[C2] - M[C1])/(M[C1]**2)

    # Sort D1 and D2 in descending order
    D1_sorted = D1.sort_values(ascending=False)
    D2_sorted = D2.sort_values(ascending=False)

    # Get the top K colors
    top_k_D1_colors = D1_sorted.index[:K].tolist()

    # Find the minimum value in top_k_D1_colors
    min_D1 = min([D1_sorted[color] for color in top_k_D1_colors])

    # Get the colors in D2_sorted that are less than min_D1
    valid_D2_colors = [color for color in D2_sorted.index if D1_sorted[color] < min_D1]

    # Get the top K colors from valid_D2_colors
    top_k_D2_colors = valid_D2_colors[:K]

    return top_k_D1_colors, top_k_D2_colors

# Test the function with two concepts and K=5
C1 = 'happy'
C2 = 'angry'
K = 4
top_k_colors_C1 = get_top_k_colors(C1, C2, K)
top_k_colors_C2 = get_top_k_colors(C2, C1, K)

print(f'Top {K} colors for {C1} vs {C2}: {top_k_colors_C1}')

import matplotlib.pyplot as plt
import matplotlib.colors as mcolors

# Get all the matplotlib colors
all_colors = mcolors.CSS4_COLORS

# Create a list of unique colors in your data
unique_colors = M.index.unique()

# Create a dictionary that maps the colors from M to actual matplotlib colors
color_dict = {color: all_colors[key] for color, key in zip(unique_colors, all_colors)}

def plot_weights(C1, top_k_D1_colors, top_k_D2_colors):
    # Get the weights for the top colors for D1 and D2
    D1_weights = M.loc[top_k_D1_colors, C1]
    D2_weights = M.loc[top_k_D2_colors, C1]

    # Create a new figure
    #plt.figure(figsize=(10, 6))

    # Create a bar plot for D1 and D2
    plt.bar(D1_weights.index, D1_weights, color=[color_dict[color] for color in top_k_D1_colors])
    plt.bar(D2_weights.index, D2_weights, color=[color_dict[color] for color in top_k_D2_colors])
    
    plt.xlabel('Weight')
    plt.title(f'{C1}')
    plt.legend()

    # Show the plot
    plt.tight_layout()
    #plt.show()

# Test the function with the previously computed top colors
# Create a new figure
plt.figure(figsize=(10, 6))

# Create a bar plot for D1
plt.subplot(2, 1, 1)
plot_weights(C1, *top_k_colors_C1)
plt.subplot(2, 1, 2)
plot_weights(C2, *top_k_colors_C2)
plt.tight_layout()

def plot_rates(concept):
    # Get the rates for the concept
    rates = M[concept]

    # Sort the rates in descending order
    rates_sorted = rates.sort_values(ascending=False)

    # Create a new figure
    # plt.figure(figsize=(10, 6))

    # Create a bar plot for the rates
    plt.bar(rates_sorted.index, rates_sorted, color=[color_dict[color] for color in rates_sorted.index])
    
    plt.xlabel('Rate')
    plt.title(f'Rates for all colors for {concept}')

    # Show the plot
    plt.tight_layout()
    # plt.show()

# Test the function with the concept 'happy'

plt.figure(figsize=(10, 6))

# Create a bar plot for D1
plt.subplot(2, 1, 1)
plot_rates('tree')
plt.subplot(2, 1, 2)
plot_rates('banana')
plt.tight_layout()