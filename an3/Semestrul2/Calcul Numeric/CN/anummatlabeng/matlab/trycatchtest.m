done = false;
while ~done
    state = input('Enter a valid statement: ','s');
    try
        eval(state);
        done = true;
    catch
        err=lasterror;
        disp('That was not a valid statement! Look:')
        disp(err.identifier)
        disp(err.message)
    end
end