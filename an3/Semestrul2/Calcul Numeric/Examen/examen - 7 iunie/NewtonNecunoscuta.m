% function [z, k] = NewtonFaraMult(f, fd, x0, iter_max)
%     if nargin < 5; iter_max = 500; end
%     er = 0;
%     ea = 1e-3;
% 
%     x_prev_2 = x0;
%     x_prev_1 = x_prev_2 - fd(x_prev_2) \ f(x_prev_2);
%     x_prev = x_prev_1 - fd(x_prev_1) \ f(x_prev_1);
% 
%     m_numitor = log(abs((x_prev_1 - x_prev) / (x_prev_2 - x_prev)));
%     m_numarator = log(abs(f(x_prev_1) / f(x_prev_2)));
%     m = round(m_numarator / m_numitor);
% 
% 
%     for k = 1 : iter_max
%         x_curr = x_prev - m * fd(x_prev) \ f(x_prev);
%         if norm(x_curr - x_prev, inf) < ea + er * norm(x_curr, inf) % ok
%             z = x_curr;
%             return
%         end
% 
%         x_prev_2 = x_prev_1;
%         x_prev_1 = x_prev;
%         x_prev = x_curr;
%         
%         m_numitor = log(abs((x_prev_1 - x_prev) / (x_prev_2 - x_prev)));
%         m_numarator = log(abs(f(x_prev_1) / f(x_prev_2)));
%         m = round(m_numarator / m_numitor);
%     end
% 
%     error('numarul maxim de iteratii depasit') % eroare
% end

function [z, k] = NewtonNecunoscuta(f, fd, x0, err, iter_max)
    %f - functia
    %fd - derivata
    %x0 - aproximatia initiala 
    %err - eroarea
    %iter_max - numarul maxim de iteratii
    %Iesire
    %z - aproximatia radacinii
    %ni - numarul de iteratii
    
    if (nargin < 4); err = 1e-6; end
    if (nargin < 5); iter_max = 500; end

    m = 1;
    x_prev = x0;
    x_prev1 = x0;
    for k = 1 : iter_max
        x_curr = x_prev - m * f(x_prev) / fd(x_prev);
        if (abs(x_curr - x_prev) < err)
            z = x_curr; 
            return
        end
        if (x_prev1 ~= x_prev && f(x_prev) ~= 0)
            m = log(abs(f(x_prev)/f(x_prev1)))/log(abs((x_prev-x_curr)/(x_prev1-x_curr)));
        end
        x_prev1 = x_prev;
        x_prev= x_curr; 
    end
    error('Numar maxim de iteratii atins')
end