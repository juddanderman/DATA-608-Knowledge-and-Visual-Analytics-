# -*- coding: utf-8 -*-
import dash
import dash_core_components as dcc
import dash_html_components as html
import pandas as pd
import numpy as np
import plotly.graph_objs as go

url = 'https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture4/Data/riverkeeper_data_2013.csv'
df = pd.read_csv(url)
df['Date'] = pd.to_datetime(df['Date'])
df['EnteroCount'].str.replace('>|<', '')
df['EnteroCount'] = pd.to_numeric(df['EnteroCount'].str.replace('>|<', ''), downcast='integer')

clean = df.groupby('Site')['EnteroCount'].agg({'AvgEntero': 'mean'}).sort_values('AvgEntero').iloc[0:5]
clean['Class'] = 'Cleanest'
dirty = df.groupby('Site')['EnteroCount'].agg({'AvgEntero': 'mean'}).sort_values('AvgEntero', ascending=False).iloc[0:5]
dirty['Class'] = 'Dirtiest'
subset = clean.append(dirty).reset_index()
subset = subset.sort_values('AvgEntero', ascending=True)
subset = subset.merge(df, how='inner', on='Site')
new_df = df.loc[(df['EnteroCount'] < np.percentile(df['EnteroCount'], 95)) &
                (df['FourDayRainTotal'] < np.percentile(df['FourDayRainTotal'], 95))]

app = dash.Dash()

app.layout = html.Div(children=[
    html.H2(children='Enteroccous Levels in the Hudson River'),

    html.Div(children='''Data sourced from Riverkeeper'''),

    html.Div([
        html.Div([
            dcc.Graph(
                id='clean-dirty',
                figure={
                    'data': [
                        {'x': subset['Site'], 'y': subset['AvgEntero'], 'type': 'bar', 'name': subset['Site']},
                    ],
                    'layout': go.Layout(
                        title='Cleanest and dirtiest test sites',
                        yaxis={'type': 'log', 'title': 'Average Enterococcus cell count'},
                        margin={'l': 400, 'b': 100, 't': 100, 'r': 200}
                    )
                }
            )
        ]),
        html.Div([        
            dcc.Graph(
                id='freq-testing',
                figure={
                    'data': [
                        go.Scatter(
                            x=subset['Date'],
                            y=subset['Site'],
                            mode='markers'
                        )
                    ],
                    'layout': go.Layout(
                        title='Frequency of testing across sites',
                        margin={'l': 400, 'b': 100, 't': 100, 'r': 200},
                        hovermode='closest'
                    )
                }
            )
        ]),
        html.Div([        
            dcc.Graph(
                id='entero-v-rain',
                figure={
                    'data': [
                        go.Scatter(
                            x=new_df['FourDayRainTotal'],
                            y=new_df['EnteroCount'],
                            mode='markers'
                        )
                    ],
                    'layout': go.Layout(
                        title='Enterococcus cell count vs. four day rain total',
                        xaxis={'title': 'Four day rain total'},
                        yaxis={'title': 'Enterococcus cell count'},
                        margin={'l': 400, 'b': 100, 't': 100, 'r': 200},
                        hovermode='closest'
                    )
                }
            )
        ])
    ])
])

if __name__ == '__main__':
    app.run_server(debug=True)
