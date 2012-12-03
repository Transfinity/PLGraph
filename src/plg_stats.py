def load_lang_data(path='../data/comparison.csv') :
    import csv
    raw_data = []
    with open(path, 'rb') as csvfile :
        reader = csv.reader(csvfile, delimiter='\t')
        for line in reader :
            raw_data.append(line)

    labeled_data = []
    labels = raw_data[0]
    for line in raw_data[1:] :
        labeled_line = {}
        for i in range(0, len(labels)) :
            labeled_line[labels[i]] = line[i]
        labeled_data.append(labeled_line)

    return labeled_data

def remove_lang(data, lang) :
    rtn = []
    for vector in data :
        if vector['Language'] != lang :
            rtn.append(vector)
    return rtn

def extract_vals(data, keys) :
    """ data should be a list of dictionaries, x_key and y_key
    are the two keys to parse with. """
    import numpy as np

    values = []
    for vector in data :
        row = []
        for key in keys :
            if vector[key] == 'NA' :
                row.append(None)
            elif vector[key] == 'Yes' :
                row.append(1.0)
            elif vector[key] == 'No' :
                row.append(0.0)
            else :
                try :
                    val = float(vector[key])
                    row.append(val)
                except ValueError :
                    row.append(vector[key])

        values.append(np.array(row))

    return np.array(values)

def doublize(sarr) :
    darr = []
    for elem in sarr :
        if elem == 'Yes' :
            darr.append(1.0)
        elif elem == 'No' :
            darr.append(0.0)
        else :
            darr.append(float(elem))
    return darr

def sort_by(data, key, extras=[]) :
    # Figure out what type of thing we have
    try :
        int(data[0][key])
        elem_type = int
    except ValueError :
        try :
            float(data[0][key])
            elem_type = float
        except ValueError :
            elem_type = str

    elements = []
    for point in data :
        tup = (elem_type(point[key]), point['Language'])
        for extra_key in extras :
            tup += (point[extra_key],)
        elements.append(tup)

    elements.sort()

    return elements

def plot_fit(data, x_key, y_key, highlight=None) :
    import matplotlib.pyplot as plt
    import scipy.stats as sst
    from scipy import optimize
    import numpy as np

    if highlight == None :
        highlight = []
    elif isinstance(highlight, str) :
        highlight = [highlight]

    values = extract_vals(data, [x_key, y_key])
    x = values[:,0]
    y = values[:,1]

    fitfunc = lambda p, x: p[0]*x + p[1]
    errfunc = lambda p, x, y: fitfunc(p, x) - y

    slope_guess = (y[-1] - y[0]) / (x[-1] - x[0])
    intercept_guess = (y[-1] + y[0]) / 2 - slope_guess * (x[-1] + x[0]) / 2

    p0 = [slope_guess, intercept_guess]

    p1, success = optimize.leastsq(errfunc, p0[:], args=(x, y))

    r, p = sst.pearsonr(x, y)

    plt.xlabel(x_key)
    plt.ylabel(y_key)

    plt.plot(x, y, "rx", x, fitfunc(p1, x), 'b-')

    # Draw highlighted points
    for point in data :
        if point['Language'] in highlight :
            plt.plot(point[x_key], point[y_key], 'go')
            # TODO: add a label

    # Draw the legend
    x_coord = (x.max() - x.min()) * 3 / 4 + x.min()
    y_coord = (y.max() - y.min()) * 5 / 6 + y.min()
    plt.text(x_coord, y_coord, 'Pearson r = %.2f\nP value = %.2f' %(r, p))
    plt.show()

def mlr(data, dependent_key, independent_keys=None) :
    import ols
    import numpy as np

    if independent_keys == None:
        independent_keys = ['Age', # 'CapersJones', 'Popularity',
                'C-based', 'OO', 'Compiled', 'DynamicTyping']

    all_keys = [dependent_key]
    all_keys.extend(independent_keys)

    values = extract_vals(data, all_keys)
    y = values[:,0]
    x = values[:,1:]

    try :
        model = ols.ols(y, x, dependent_key, independent_keys)
    except Exception :
        print 'Encountered a LinAlgError!'
        print 'dumping values...'
        print '\ny values'
        print y
        print '\nx values'
        print x
        raise

    model.summary()

def anova_oneway (data, val_key, group_key, treatment_group=None) :
    """ performs a 1-way anova (equivalent to a student's t-test for
    only two groups)"""

    import scipy.stats as sst
    import numpy as np
    import pylab

    # First we must divide up the groups
    groups = {}

    if treatment_group == None :
        pylab.title('%s grouped by %s' %(val_key, group_key))
        for vector in data :
            if vector[group_key] not in groups.keys() :
                print 'Found new group:', vector[group_key]
                groups[vector[group_key]] = []

            groups[vector[group_key]].append(vector[val_key])
    else :
        pylab.title('%s with and without treatment %s for %s' %(val_key, treatment_group, group_key))
        groups['treatment'] = []
        groups['other'] = []
        for vector in data :
            if vector[group_key] in treatment_group :
                groups['treatment'].append(vector[val_key])
            else :
                groups['other'].append(vector[val_key])

    for g in groups.keys() :
        groups[g] = np.array(doublize(groups[g]))

        print 'Group', g
        print groups[g]

    # Do the statistics
    f, p = sst.f_oneway(*groups.values())

    # Plot the graph
    labels = groups.keys()
    for l in range(0, len(labels)) :
        label = labels[l]
        pylab.plot([l]*len(groups[label]), groups[label], "x")
        mean = groups[label].mean()
        size = len(groups[label])
        pylab.plot(l, mean, "ro")
        pylab.text((l+.1), mean, 'Group "%s"\nMean %.4f\n(%d values)'
                %(label, mean, size))

    pylab.ylabel(val_key)
    pylab.xlabel(group_key)
    pylab.xticks(range(-1, len(labels)+1), [''] + labels + [''])

    # Print the statistics
    yt = pylab.yticks()[0]
    info_y = yt[0] + (yt.max() - yt.min()) * 5/6
    info_x = -.9
    pylab.text(info_x, info_y, 'F value %.2f\np value %.4f' %(f, p))

    pylab.show()

    return f, p
